# Reducer property testing

The reducer property suite builds valid, shrinkable multi-asset traces and runs them through the
same public transitions used by normal replay. A trace contains four to fourteen synchronized
slices with independent prices, volumes, and EUR/USD marks. Generated strategy commands include
direct market and limit orders, cancellations selected from the live working-order inventory,
quantity and weight targets, and metrics. Matching turns eligible orders into partial or complete
fills. Traces may also include cash dividends and one split, and vary participation, leverage,
initial margin, and maintenance margin.

Direct-order generation consults the current strategy context. It avoids overlapping working
orders for the same instrument and bounds position-reducing orders so one fill cannot cross through
zero. Invalid business requests may still be generated intentionally: the reducer must express
those as deterministic rejection events rather than corrupting state or escaping the transition.

## Checked properties

After every completed slice, `test/test_reducer_properties.ml` checks:

- cash, equity, net value, long and short value, gross exposure, cost basis, realized and
  unrealized P&L, dividends, and fees against their per-currency and per-position attributions;
- native and base-currency conversions, including the defined per-field rounding boundaries;
- initial and maintenance requirements, excess, and margin-call state against the configured risk
  model;
- order quantities, fill totals, statuses, active inventory, fill notionals, and fill ownership;
- contiguous engine sequences, derived event IDs, canonical causal references, and the rule that
  every cause names an earlier event.

A second property drives every generated trace through both `Engine.Make` and
`Engine.Interactive`. Audit records are compared as exact serialized bytes after each slice and at
completion; account and order snapshots are compared independently.

## Reproducing failures

QCheck prints its random seed and shrinks a failure by removing slices and commands and reducing
numeric inputs. The final report includes the smallest scenario-like JSON trace it found. Re-run a
seed through the complete test gate with:

```sh
QCHECK_SEED=123456 make test
```

Increase the generated case count without changing the checked-in defaults with:

```sh
QCHECK_SEED=123456 REDUCER_PROPERTY_CASES=5000 make test
```

The seed reproduces generation; the printed shrunk trace is the durable debugging artifact when
generator behavior later changes.
