# Execution model

The simulator consumes completed OHLCV bars. It produces deterministic proposed fills from active
orders and then lets the engine apply those fills through the OMS and account.

## Eligibility

An order records the bar sequence after which it is eligible. The matcher requires:

```text
eligible_after_bar_sequence < current source_sequence
created_at <= current bar start_at
```

This rule prevents an order emitted in response to a completed bar from filling inside that same
bar or at a later bar open that predates the order. The simulator skips an overlapping bar when
the order was created after its start because completed OHLCV data cannot establish whether an
intrabar price occurred before or after the order.

The scenario validator also requires each scheduled order-changing intent to be received by the
start of its instrument's next bar. Without this condition, a later callback could cancel or
replace an order after a future open occurred but before that completed bar reached the reducer.
This is a deliberate completed-bar limitation, not an event-queue simulation.

## Market orders

A market order executes at the open of its first eligible bar. Participation capacity can limit
the fill. After that matching attempt, the engine cancels any remainder with reason `market_ioc`.

The model does not carry an unfilled market order to a later opening price.

## Limit orders

For a buy limit `L`:

1. If `open <= L`, fill at `open`.
2. Otherwise, if `low <= L`, fill at `L`.
3. Otherwise, do not fill.

For a sell limit `L`:

1. If `open >= L`, fill at `open`.
2. Otherwise, if `high >= L`, fill at `L`.
3. Otherwise, do not fill.

A partially filled limit remains working. The model treats limit orders as GTC within the replay.

The open rule gives deterministic gap-price improvement. The touch rule is optimistic because a
bar contains no queue, path, or available-size evidence at the limit.

## Capacity and order priority

Missing bar volume means unlimited simulated capacity. Otherwise:

```text
raw capacity = floor(volume × participation_bps / 10,000)
capacity = raw capacity rounded down to the instrument lot size
```

Eligible crossing orders share that capacity in ascending order-creation sequence and order-ID
order. A fill never exceeds the order remainder or remaining bar capacity.

## Prices and quantities

Prices and money use six decimal places stored in checked `int64` values. Quantities use
nonnegative whole units. Orders must align with the configured lot size. Limit prices and every
OHLC value used for execution must align with the configured tick size.

Scenario JSON encodes these exact values as strings. This avoids JSON's portable integer precision
limit and rejects non-finite floating-point inputs.

## Fees

Each fill pays:

```text
fixed_fee + ceil(fill_notional × fee_bps / 10,000)
```

The variable result rounds up to the next money micro-unit. Each partial fill pays its own fixed
fee, so fill fragmentation affects total cost.

## Accounting

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

Closing a position removes the exact remaining basis. A partial sale rounds removed basis down to
one money micro-unit and leaves the exact remainder on the open position.

Valuation uses the latest completed close for every held instrument:

```text
market value   = sum(mark × quantity)
unrealized P&L = market value - remaining cost basis
equity         = cash + market value
```

The prototype permits negative cash. It has no buying-power or margin policy.
