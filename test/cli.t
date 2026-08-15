  $ ../bin/main.exe --version
  0.1.0-dev

  $ ../bin/main.exe --capabilities
  {"engine_version":"0.1.0-dev","scenario_contract_versions":["1"],"journal_contract_versions":["1"],"scenario_formats":["json","jsonl"],"journal_formats":["jsonl"],"execution_models":["completed_bar_v1"]}

  $ ../bin/main.exe --validate-only --input ../contracts/v1/fixtures/demo.scenario.json
  valid run=demo instruments=1 schedule=2 slices=4 scenario_sha256=a782fbbee8b89332ee6491d9d9be36bf7f2aaacbd440d30c20ed033fd7566d19

  $ ../bin/main.exe --validate-only --input-format jsonl --input ../contracts/v1/fixtures/demo.scenario.jsonl
  valid run=demo instruments=1 schedule=2 slices=4 scenario_sha256=d83b7c23d5e44793f9b71c542738cf4fed0612117e8399a61059677974f3382c

  $ ../bin/main.exe --input-format jsonl --input ../contracts/v1/fixtures/demo.scenario.jsonl --journal streamed.journal.jsonl
  run=demo audits=20 orders=3 active=0 filled=2 rejected=0
  cash=9793.576 equity=10005.576 realized=2.562445 unrealized=3.013555 fees=2.424
  journal=streamed.journal.jsonl
  $ wc -l < streamed.journal.jsonl
  20

  $ head -n 5 ../contracts/v1/fixtures/demo.scenario.jsonl > truncated.scenario.jsonl
  $ ../bin/main.exe --validate-only --input-format jsonl --input truncated.scenario.jsonl
  trading-engine: scenario_end must terminate the scenario stream
  [123]

  $ sed 's/"open": "100"/"open": "100.001"/' ../contracts/v1/fixtures/demo.scenario.json > invalid-tick.json
  $ ../bin/main.exe --validate-only --input invalid-tick.json
  trading-engine: bar price is not aligned to the instrument tick size
  [123]

  $ ../bin/main.exe --input ../contracts/v1/fixtures/demo.scenario.json
  trading-engine: --journal is required unless --validate-only is set
  [123]

  $ ../bin/main.exe --validate-only --input ../contracts/v1/fixtures/demo.scenario.json --journal validation.journal.jsonl
  trading-engine: --journal cannot be used with --validate-only
  [123]

  $ test ! -e validation.journal.jsonl
