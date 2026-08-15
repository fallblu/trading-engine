  $ ../bin/main.exe --validate-only --input ../examples/demo.json
  valid run=demo instruments=1 schedule=2 slices=4 scenario_sha256=04ec1d156ccb9748a3be9e451b55afe8981b92871d07735678c6eb21622f8745

  $ sed 's/"open": "100"/"open": "100.001"/' ../examples/demo.json > invalid-tick.json
  $ ../bin/main.exe --validate-only --input invalid-tick.json
  trading-engine: bar price is not aligned to the instrument tick size
  [123]

  $ ../bin/main.exe --input ../examples/demo.json
  trading-engine: --journal is required unless --validate-only is set
  [123]

  $ ../bin/main.exe --validate-only --input ../examples/demo.json --journal validation.journal.jsonl
  trading-engine: --journal cannot be used with --validate-only
  [123]

  $ test ! -e validation.journal.jsonl
