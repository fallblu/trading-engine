# Execution model

The engine selects a compiled execution module by the scenario's stable `execution.model` name.
Contract v2 advertises and accepts `completed_bar_v1`; embedders can inject another module through
the typed engine configuration without introducing runtime shared-library loading. The selected
name is repeated in both terminal audit records.

The completed-bar model consumes synchronized slices of OHLCV bars. Every slice contains exactly
one bar for each configured instrument and produces one matching batch and one closing valuation.

## Eligibility

An order records the slice after which it is eligible. The matcher requires:

```text
eligible_after_slice_sequence < current slice_sequence
created_at <= current slice start_at
```

This prevents an order emitted from a completed slice from filling inside that slice or at an open
that predates the order. The parser also requires scheduled order-changing intents to arrive by
the next slice start.

## Portfolio targets

`target_weights` and `target_quantities` contain one target for every configured instrument. A
weight request:

1. Values the current account at all synchronized slice closes.
2. Multiplies that equity by each exact weight.
3. Divides by the corresponding closing price.
4. Rounds down to the instrument lot.

Weights are nonnegative and sum to at most one. Quantity targets must align with their lots. Every
desired quantity must stay within `max_position`.

The computed desired quantities persist. After each slice, the engine compares them with actual
positions and submits at most one market order per instrument. Each order is capped at
`max_order_quantity` and rounded down to a lot, so large targets advance in bounded chunks. A
market remainder is IOC, but the desired target is retried after a later slice until reached or
superseded.

## Market and limit prices

A market order executes at the open of its first eligible slice.

For a buy limit `L`:

1. If `open <= L`, fill at `open`.
2. Otherwise, if `low <= L`, fill at `L`.
3. Otherwise, do not fill.

Sell limits use the symmetric open/high rule. Limit remainders remain GTC. The open rule gives
deterministic gap improvement. The touch rule is optimistic because completed bars contain no
queue, path, or available-size evidence at the limit.

## Capacity and priority

Missing volume means unlimited simulated capacity. Otherwise:

```text
raw capacity = floor(volume × participation_bps / 10,000)
capacity = raw capacity rounded down to the instrument lot size
```

All eligible sells are ordered before buys across the slice. Orders within a side use ascending
creation sequence and order ID. Each instrument has its own shared capacity, so a sell consumes
that instrument's capacity before a competing buy.

## Cash and fees

Each proposed fill pays:

```text
fixed_fee + ceil(fill_notional × fee_bps / 10,000)
```

Sell fills are applied before buys across the slice, making their net proceeds available to later
buys. For each buy, the engine finds the largest whole-lot quantity whose actual-price notional
plus fee fits current cash. It emits `cash_limited` when this is below the execution proposal. A
zero affordable quantity produces no fill. Only the quantity actually applied consumes shared
slice capacity, so cash-clipped capacity remains available to later eligible buys for the same
instrument. Account transitions independently reject any fill that would make cash negative,
including a sell whose fee exceeds cash plus proceeds.

Each partial fill pays its own fixed fee, so fragmentation affects total cost.

## Exact values

Prices, weights, and money use six decimal places stored in checked `int64` values. Quantities use
nonnegative whole units. Scenario strings must use the canonical shortest representation: `1`,
`1.25`, and `0.000001` are valid; `01`, `1.0`, and negative zero are not.

Orders align with lot size. Limit prices and executable OHLC values align with tick size.

## Accounting and valuation

For a buy with notional `N` and fee `F`:

```text
cash       -= N + F
quantity   += fill quantity
cost basis += N + F
```

For a sell:

```text
cash          += N - F
removed basis  = proportional average cost
realized P&L  += N - F - removed basis
```

Closing a position removes its exact remaining basis. A partial sale rounds removed basis down to
one money micro-unit and leaves the exact remainder open.

Valuation uses all synchronized closes:

```text
market value   = sum(mark × quantity)
unrealized P&L = market value - remaining cost basis
equity         = cash + market value
```

Each valuation also emits one deterministic attribution row per marked instrument. Row market
value, basis, realized P&L, unrealized P&L, and cumulative fees sum exactly to the corresponding
account totals. Closed instruments retain cumulative realized P&L and fees with zero quantity and
basis.
