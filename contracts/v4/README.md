# Trading Engine contract v4

This directory is the authoritative v4 process and file contract shared by Trading Engine and
its clients. Version 3 remains available under `contracts/v3` during the client transition.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- The files under `fixtures/` form the canonical valid conformance corpus.
- `fill-clipped.scenario.json` and its journal exercise a leverage-limited partial fill.

Version 4 replaces the ambiguous `margin_limited` journal event with `fill_clipped`. Its versioned,
exhaustive reason identifies the limiting policy and a typed threshold alongside the proposed and
permitted quantities. Every v4 scenario, stream record, and journal record carries
`"contract_version": "4"`; consumers reject missing or unsupported versions before interpreting
the remainder of a document. The scenario shape is otherwise unchanged from v3.

`fill_clipped.payload.reason.version` is `"1"`. Its exhaustive policy and threshold pairs are:

| Policy | Threshold unit | Threshold value |
| --- | --- | --- |
| `max_order_quantity` | `quantity` | Configured maximum order quantity |
| `max_long_position` | `quantity` | Configured maximum long position |
| `max_short_position` | `quantity` | Configured maximum short position magnitude |
| `max_gross_exposure` | `money` | Configured maximum gross exposure |
| `max_leverage` | `ratio` | Configured maximum leverage |
| `initial_margin` | `basis_points` | Configured initial-margin basis points |

The event also records `order_id`, `instrument_id`, `proposed_quantity`, `permitted_quantity`, and
the proposed fill `price`. A consumer must reject unknown reason versions, policies, threshold
units, and policy/unit combinations.
