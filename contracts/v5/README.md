# Trading Engine contract v5

This directory is the authoritative v5 process and file contract shared by Trading Engine and its
clients. Versions 4 and 3 remain readable during their client transitions.

- `scenario.schema.json` validates batch replay inputs.
- `scenario-stream.schema.json` validates each JSON Lines scenario-stream record.
- `journal.schema.json` validates each JSON Lines audit record.
- The files under `fixtures/` form the canonical valid conformance corpus.
- `fill-clipped.scenario.json` and its journal exercise a leverage-limited partial fill.

Version 5 adds explicit immutable venue-calendar snapshots. Each calendar has stable venue and
calendar identities, a calendar contract version, explicit instrument membership, and ordered
date policies. A date is either a holiday or an open session named as regular or early-close.
Open sessions contain absolute timestamp intervals for configured premarket, opening-auction,
regular, closing-auction, and postmarket phases.

Calendar producers resolve local civil time, time-zone database versions, daylight-saving rules,
and clock changes before creating a scenario. The reducer receives only absolute instants. Missing
date policies are errors and must never be inferred from weekdays or adjacent sessions. Runtime
validation also rejects duplicate calendar identities, overlapping instrument membership, missing
instrument coverage, unordered or overlapping phases, holidays with phases, and open sessions
without a regular phase.

Every v5 scenario, stream record, and journal record carries `"contract_version": "5"`.
