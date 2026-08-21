  $ ../bin/main.exe --version
  0.1.0-dev

  $ ../bin/main.exe --capabilities
  {"engine_version":"0.1.0-dev","scenario_contract_versions":["3"],"journal_contract_versions":["3"],"scenario_formats":["json","jsonl"],"journal_formats":["jsonl"],"execution_models":["completed_bar_v1"],"strategy_protocol_versions":["3"]}

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

  $ ../bin/main.exe --input ../contracts/v3/fixtures/demo.scenario.json --journal ignored.journal.jsonl --strategy-timeout 5
  trading-engine: --strategy-arg, --strategy-timeout, and --strategy-transcript require --strategy-executable
  [123]
  $ test ! -e ignored.journal.jsonl

  $ mkdir external
  $ ../bin/main.exe --input ../contracts/strategy/v3/fixtures/external.scenario.json --journal external/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-transcript external/run.strategy.jsonl --strategy-timeout 5
  run=external-demo audits=10 orders=1 active=0 filled=1 rejected=0
  cash=9794 equity=10008 gross=214 realized=0 unrealized=8 fees=0
  journal=external/run.journal.jsonl
  strategy_transcript=external/run.strategy.jsonl

  $ python3 -c 'from pathlib import Path; print(len(Path("external/run.journal.jsonl").read_text().splitlines()), len(Path("external/run.strategy.jsonl").read_text().splitlines()))'
  10 14
  $ diff -u ../contracts/strategy/v3/fixtures/external.strategy.jsonl external/run.strategy.jsonl

  $ mkdir callback-ordering
  $ ../bin/main.exe --input ../contracts/strategy/v3/fixtures/external.scenario.json --journal callback-ordering/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-arg cancel-next --strategy-transcript callback-ordering/run.strategy.jsonl --strategy-timeout 5
  run=external-demo audits=10 orders=2 active=0 filled=1 rejected=0
  cash=9897 equity=10004 gross=107 realized=0 unrealized=4 fees=0
  journal=callback-ordering/run.journal.jsonl
  strategy_transcript=callback-ordering/run.strategy.jsonl
  $ python3 -c 'import json; journal=[json.loads(line) for line in open("callback-ordering/run.journal.jsonl")]; transcript=[json.loads(line) for line in open("callback-ordering/run.strategy.jsonl")]; events=[record["event_type"] for record in journal]; cancellations=[record["payload"]["reason"] for record in journal if record["event_type"] == "order_cancelled"]; requests=[record["message"]["payload"] for record in transcript if record["direction"] == "engine_to_strategy" and record["message"]["message_type"] == "event"]; fill=next(request for request in requests if request["event"]["type"] == "fill_received"); following=requests[requests.index(fill) + 1]; print(events.count("fill_applied"), cancellations, events.count("intent_rejected")); print(len(fill["context"]["working_orders"]), len(following["context"]["working_orders"])); print(fill["context"]["latest_bars"][0]["close"], fill["context"]["portfolio"]["positions"][0]["mark"])'
  1 ['strategy_requested'] 0
  1 0
  107 107

  $ mkdir failed-external
  $ ../bin/main.exe --input ../contracts/strategy/v3/fixtures/external.scenario.json --journal failed-external/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-arg stall --strategy-transcript failed-external/run.strategy.jsonl --strategy-timeout 0.01
  trading-engine: strategy initialization: external strategy timed out
  [123]
  $ test ! -e failed-external/run.journal.jsonl
  $ test -e failed-external/run.journal.jsonl.partial
  $ test ! -e failed-external/run.strategy.jsonl
  $ test -e failed-external/run.strategy.jsonl.partial

  $ check_strategy_failure () {
  >   mode="$1"
  >   expected="$2"
  >   directory="fault-$mode"
  >   mkdir "$directory"
  >   output=$(../bin/main.exe --input ../contracts/strategy/v3/fixtures/external.scenario.json --journal "$directory/run.journal.jsonl" --strategy-executable ./fake_strategy.py --strategy-arg "$mode" --strategy-transcript "$directory/run.strategy.jsonl" --strategy-timeout 5 2>&1)
  >   status=$?
  >   test "$status" -eq 123 || return 1
  >   case "$output" in *"$expected"*) ;; *) return 1 ;; esac
  >   test ! -e "$directory/run.journal.jsonl" || return 1
  >   test -e "$directory/run.journal.jsonl.partial" || return 1
  >   test ! -e "$directory/run.strategy.jsonl" || return 1
  >   test -e "$directory/run.strategy.jsonl.partial" || return 1
  >   echo "$mode: rejected"
  > }
  $ check_strategy_failure eof "closed stdout"
  eof: rejected
  $ check_strategy_failure bad-sequence "expected strategy sequence"
  bad-sequence: rejected
  $ check_strategy_failure error "fixture failure"
  error: rejected
  $ check_strategy_failure extra-output "data after its stopped response"
  extra-output: rejected
  $ check_strategy_failure nonzero "exited with code 9"
  nonzero: rejected
  $ check_strategy_failure malformed "invalid strategy response JSON"
  malformed: rejected
  $ check_strategy_failure oversized "exceeds the maximum message size"
  oversized: rejected
  $ check_strategy_failure wrong-version "unsupported strategy protocol version"
  wrong-version: rejected
  $ check_strategy_failure unknown-field "unknown or missing fields"
  unknown-field: rejected

  $ mkdir external-stream
  $ ../bin/main.exe --input-format jsonl --input ../contracts/strategy/v3/fixtures/external.scenario.jsonl --journal external-stream/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-transcript external-stream/run.strategy.jsonl --strategy-timeout 5
  run=external-demo audits=10 orders=1 active=0 filled=1 rejected=0
  cash=9794 equity=10008 gross=214 realized=0 unrealized=8 fees=0
  journal=external-stream/run.journal.jsonl
  strategy_transcript=external-stream/run.strategy.jsonl
