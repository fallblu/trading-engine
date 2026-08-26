# Execution model

The scenario selects one compiled model by `execution.model`. Each model uses strict configuration
version `"1"`, declares its required fields through `--capabilities`, and shares the same order,
risk, fee, settlement, accounting, and audit pipeline.

## Available models

| Model | Market evidence |
| --- | --- |
| `completed_bar_v1` | Next eligible open and optimistic intrabar limit touch |
| `completed_bar_next_open_v1` | Later marketable opens with explicit spread and impact |
| `completed_bar_adverse_touch_v1` | Opens or one-tick adverse trade-through with costs |
| `quote_trade_v1` | Displayed quotes and aggressor-classified trades |
| `order_book_v1` | Bounded level-two snapshots and contiguous updates |

Completed bars remain mandatory for synchronized valuation. Conservative models require fixed
half-spread and linear participation-impact policies. Quote/trade and order-book models use causal
availability, receipt, and ingest ordering and never infer hidden liquidity.

## Eligibility and order lifetime

An order becomes eligible only when both conditions hold:

```text
eligible_after_slice_sequence < current slice_sequence
created_at <= current slice start_at
```

Market orders attempt the next eligible evidence and cancel any IOC remainder. Limit orders use
their configured time in force. Stop orders activate when their trigger is observed. FOK requires
the full quantity to pass liquidity and risk checks before any fill is applied.

Persistent portfolio targets are reconciled in lot-aligned, maximum-order-sized attempts until the
target is reached or replaced. A sign change flattens before opening the opposite side.

## Capacity, priority, and callbacks

Completed-bar capacity is volume multiplied by `participation_bps`, rounded down to the instrument
lot. Quote and book models use only displayed or causally consumed liquidity. Liquidation orders
run first; within an origin class sells precede buys, followed by FIFO creation order.

After each fill, the reducer applies the strategy callback response before examining the next
eligible order. A cancellation can therefore remove a later same-slice order. Newly submitted
orders wait for another slice.

## Risk and fees

Admission reserves every active order's remaining quantity. Fill-time checks use the actual price
and search for the largest permitted lot-aligned quantity under instrument position/notional,
portfolio gross exposure/leverage, margin, locate, and maximum-order limits. Exposure-reducing
fills remain available. A clipped proposal emits a typed `fill_clipped` reason.

Each instrument has exactly one fee schedule. Components may be fixed, notional basis points, or
per-unit; use explicit currency, rounding, and maker/taker applicability; and may include schedule
minimums, maximums, or rebates. FX conversion and every adjustment are retained in attribution.

## Financing, settlement, and lifecycle

Effective-time observations drive short availability, borrow charges, recalls, and per-currency
credit or debit interest. Settlement instructions use explicit business calendars and lags, with
configured settled or total cash and position availability.

Corporate actions run before matching. Splits adjust positions, targets, and working orders;
distributions allocate basis and fractional treatment explicitly. Lifecycle events preserve stable
instrument identity across symbol changes and deterministically cancel or cash out terminal assets.

## Accounting and valuation

The engine uses signed average-cost accounting in native quote currencies and converts every cash,
position, basis, P&L, and fee attribution to the scenario base currency. The core identities are:

```text
net market value = sum(base FX × mark × signed quantity)
gross exposure   = long market value + absolute short market value
unrealized P&L   = net market value - remaining base cost basis
equity           = base cash + net market value
```

Valuations include initial and maintenance margin. A maintenance breach cancels working orders,
clears targets, and creates bounded liquidation orders until positions are flat.
