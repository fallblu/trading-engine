# Execution model

The engine selects a compiled execution module by the scenario's stable `execution.model` name.
Contract v3 advertises and accepts `completed_bar_v1`; embedders can inject another module through
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

Weights and quantities are signed. Gross absolute weight must stay within `max_leverage`, quantity
targets must align with their lots, and every desired quantity must stay within the configured long
or short position limit. A target that changes sign is reached causally: flatten first, then open
the opposite side on a later attempt.

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

Eligible liquidation orders are ordered before all other orders across the slice. Within the
liquidation and ordinary origin classes, sells precede buys; orders within a side then use
ascending creation sequence and order ID. Each instrument has its own shared capacity, so the
higher-priority order consumes that instrument's capacity first.

## Corporate actions and borrow

Corporate actions are ordered by action ID and applied before matching. A split scales the signed
position, persistent quantity target, and each active order by its exact numerator/denominator
ratio. It inversely scales limit prices and preserves total position basis. If the adjusted order
cannot satisfy the configured lot or tick, the slice fails instead of silently rounding. Each
changed order emits `order_adjusted` with causal links to both the original order and split.
Unit-based risk limits do not scale with a split. Adjusted positions and persistent targets are
grandfathered: fills may reduce an out-of-limit absolute position but may not increase it, and
reconciliation orders remain bounded by the configured maximum order quantity. An adjusted active
order may exceed that maximum, but no individual fill may do so.

A cash dividend multiplies the pre-match signed position by its per-unit amount. It credits a long
or debits a short in the instrument's quote-currency ledger and records realized dividend P&L.
After actions, each open short accrues a quote-currency borrow fee from the slice open mark and the
exact `start_at`/`end_at` duration using a 365-day basis. Positive fees round upward to one money
micro-unit.

## Risk-limited fills and fees

Each proposed fill pays:

```text
fixed_fee + ceil(fill_notional × fee_bps / 10,000)
```

Liquidation proposals are processed first, followed by sells and then buys within each origin
class. For each proposal, the engine searches for the largest lot-aligned quantity whose signed
post-fill position is within the long/short cap and whose fill quantity is no greater than the
maximum order quantity.
When absolute exposure increases, the projected account must also satisfy maximum gross exposure,
maximum leverage, and initial margin. Reductions in absolute exposure are permitted without a new
initial-margin test. A clipped proposal emits `margin_limited`; a zero permitted quantity produces
no fill. Only the applied quantity consumes shared slice capacity.

This bounded-fill policy preserves split-adjusted GTC limit orders: an oversized remainder may
fill over multiple slices. Market orders remain IOC, so they fill at most one bounded quantity and
cancel any remainder after their eligible slice.

Each partial fill pays its own fixed fee, so fragmentation affects total cost.

## Exact values

Prices, weights, quantities, FX rates, and money use six decimal places stored in checked `int64`
values. Signed quantities are used for positions and targets; submitted orders and fills retain a
positive quantity plus a side. Scenario strings use the canonical shortest representation: `1`,
`1.25`, `-0.5`, and `0.000001` are valid; `01`, `1.0`, excess precision, and negative zero are not.

Orders align with lot size. Limit prices and executable OHLC values align with tick size.

## Accounting and valuation

For a buy that opens or increases a long with notional `N` and fee `F`:

```text
cash       -= N + F
quantity   += fill quantity
cost basis += N + F
```

For a sell that reduces a long:

```text
cash          += N - F
removed basis  = proportional average cost
realized P&L  += N - F - removed basis
```

Opening a short credits `N - F` to cash and records its cost basis as the negative net proceeds.
Covering a short debits `N + F`; realized P&L is the removed negative basis minus that cover cost.
One fill may reduce a position to zero but may not cross through zero. Closing a position removes
its exact remaining basis. A partial close uses proportional average basis and leaves the exact
remainder open.

The account maintains a signed cash ledger for every scenario currency. Each slice supplies a
complete currency-to-base FX vector, with base rate one. Valuation converts native cash, market
value, basis, P&L, and fees into the base reporting currency using the current marks:

```text
net market value = sum(base FX × mark × signed quantity)
gross exposure   = long market value + absolute short market value
unrealized P&L   = net market value - remaining base cost basis
equity           = base cash + net market value
```

Each valuation also emits one deterministic attribution row per marked instrument or retained
account position. A nonzero position requires a mark. A flat retained position does not; when its
mark is omitted, the row uses the canonical mark one because every mark produces zero market value
for zero quantity. Row market value, basis, realized P&L, dividend P&L, execution fees, and borrow
fees is present in both native and base values and sums exactly to the corresponding account totals.
A separate row attributes each currency ledger. Closed instruments retain cumulative realized P&L
and fees with zero quantity and basis.

The valuation includes initial and maintenance requirements and excesses. After strategy and
target processing, negative maintenance excess triggers one `margin_call`, cancels all active
orders, clears the persistent target, and submits deterministic `margin_liquidation` market orders
in instrument-ID order. Strategies receive the resulting cancellation and liquidation-order
updates before the slice valuation. Each attempt is capped by `max_order_quantity` and lot aligned.
The engine continues on later slices until every position is flat, then emits `margin_restored`
when the maintenance condition is no longer breached.
