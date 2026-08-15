  $ ../bin/main.exe --version
  0.1.0-dev

  $ ../bin/main.exe --capabilities
  {"engine_version":"0.1.0-dev","scenario_contract_versions":["3"],"journal_contract_versions":["3"],"scenario_formats":["json","jsonl"],"journal_formats":["jsonl"],"execution_models":["completed_bar_v1"]}

  $ ../bin/main.exe --validate-only --input ../contracts/v3/fixtures/demo.scenario.json
  valid run=demo instruments=1 schedule=2 slices=4 scenario_sha256=3e19fa66bc6425bb8ed7a89b338080a831dd39ea778c3c7f9e8ce1d3370fbee0

  $ ../bin/main.exe --validate-only --input-format jsonl --input ../contracts/v3/fixtures/demo.scenario.jsonl
  valid run=demo instruments=1 schedule=2 slices=4 scenario_sha256=0615019643edcd2b75c1307456d93bfba13990cb09bdc80ffc7fac98056020cd

  $ ../bin/main.exe --input-format jsonl --input ../contracts/v3/fixtures/demo.scenario.jsonl --journal streamed.journal.jsonl
  run=demo audits=20 orders=3 active=0 filled=2 rejected=0
  cash=9739.76812 equity=10004.76812 gross=265 realized=1.419136 unrealized=3.348984 fees=2.50188
  journal=streamed.journal.jsonl
  $ wc -l < streamed.journal.jsonl
  20

  $ head -n 5 ../contracts/v3/fixtures/demo.scenario.jsonl > truncated.scenario.jsonl
  $ ../bin/main.exe --validate-only --input-format jsonl --input truncated.scenario.jsonl
  trading-engine: scenario_end must terminate the scenario stream
  [123]

  $ sed 's/"open": "100"/"open": "100.001"/' ../contracts/v3/fixtures/demo.scenario.json > invalid-tick.json
  $ ../bin/main.exe --validate-only --input invalid-tick.json
  trading-engine: market prices and volumes must align with instrument increments
  [123]

  $ ../bin/main.exe --input ../contracts/v3/fixtures/demo.scenario.json
  trading-engine: --journal is required unless --validate-only is set
  [123]

  $ ../bin/main.exe --validate-only --input ../contracts/v3/fixtures/demo.scenario.json --journal validation.journal.jsonl
  trading-engine: --journal cannot be used with --validate-only
  [123]

  $ test ! -e validation.journal.jsonl
