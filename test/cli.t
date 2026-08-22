  $ ../bin/main.exe --version
  1.0.0

  $ ../bin/main.exe --capabilities
  {"engine_version":"1.0.0","scenario_contract_versions":["8","7","6","5","4","3"],"journal_contract_versions":["8","7","6","5","4","3"],"scenario_formats":["json","jsonl"],"journal_formats":["jsonl"],"execution_models":["completed_bar_v1"],"execution_model_contracts":[{"name":"completed_bar_v1","configuration_versions":["1"],"scenario_contract_versions":["8","7","6","5","4","3"],"required_fields":["version","participation_bps","fixed_fee","fee_bps"],"supported_order_types":["market","limit","stop","stop_limit"],"data_requirements":["completed_ohlcv_bars"],"limits":{"participation_bps":{"minimum":0,"maximum":10000},"fee_bps":{"minimum":0,"maximum":10000},"fixed_fee":{"minimum":"0","unit":"money"}}}],"strategy_protocol_versions":["6","5","4","3"],"resource_limits":{"version":"1","scenario_record_bytes":1048576,"strategy_message_bytes":1048576,"internal_events":100000,"catalog_instruments":4096,"intents_per_batch":4096,"artifact_record_bytes":2097152}}

  $ ../bin/main.exe --validate-only --input ../contracts/v8/fixtures/demo.scenario.json
  valid run=demo instruments=1 schedule=2 slices=4 scenario_sha256=85f7c99e0666159579c79256b3d0dc5f9c328e1275b79465fe1d4c93883e68f1

  $ ../bin/main.exe --validate-only --input-format jsonl --input ../contracts/v8/fixtures/demo.scenario.jsonl
  valid run=demo instruments=1 schedule=2 slices=4 scenario_sha256=786f38d8bd10faac03b6b15c7aa8ae0a867eedc609ca6eaa75cfd93ae3ffdcae

  $ ../bin/main.exe --input-format jsonl --input ../contracts/v8/fixtures/demo.scenario.jsonl --journal streamed.journal.jsonl --durable-artifacts
  run=demo audits=22 orders=3 active=0 filled=2 rejected=0
  cash=9846.65392 equity=10111.65392 gross=265 realized=18.965682 unrealized=7.688238 fees=3.16608
  journal=streamed.journal.jsonl
  $ wc -l < streamed.journal.jsonl
  22

  $ head -n 5 ../contracts/v8/fixtures/demo.scenario.jsonl > truncated.scenario.jsonl
  $ ../bin/main.exe --validate-only --input-format jsonl --input truncated.scenario.jsonl
  trading-engine: scenario_end must terminate the scenario stream
  [123]

  $ diagnostic=$(../bin/main.exe --diagnostic-format json --validate-only --input-format jsonl --input truncated.scenario.jsonl 2>&1); status=$?; test "$status" -eq 123; python3 - "$diagnostic" <<'PY'
  > import json
  > import sys
  > diagnostic = json.loads(sys.argv[1])
  > print(diagnostic["diagnostic_version"], diagnostic["code"], diagnostic["phase"])
  > print(diagnostic["context"]["line"], diagnostic["context"]["sequence"], diagnostic["cause"])
  > PY
  1 scenario_stream.invalid validation
  6 6 None

  $ sed 's/"open": "100"/"open": "100.001"/' ../contracts/v8/fixtures/demo.scenario.json > invalid-tick.json
  $ ../bin/main.exe --validate-only --input invalid-tick.json
  trading-engine: market prices and volumes must align with instrument increments
  [123]

  $ ../bin/main.exe --input ../contracts/v8/fixtures/demo.scenario.json
  trading-engine: --journal is required unless --validate-only is set
  [123]

  $ ../bin/main.exe --validate-only --input ../contracts/v8/fixtures/demo.scenario.json --journal validation.journal.jsonl
  trading-engine: --journal cannot be used with --validate-only
  [123]

  $ test ! -e validation.journal.jsonl

  $ ../bin/main.exe --validate-only --durable-artifacts --input ../contracts/v8/fixtures/demo.scenario.json
  trading-engine: --durable-artifacts cannot be used with --validate-only
  [123]

  $ ../bin/main.exe --input ../contracts/v8/fixtures/demo.scenario.json --journal ignored.journal.jsonl --strategy-timeout 5
  trading-engine: --strategy-arg, --strategy-timeout, and --strategy-transcript require --strategy-executable
  [123]
  $ test ! -e ignored.journal.jsonl

  $ mkdir external
  $ ../bin/main.exe --input ../contracts/strategy/v6/fixtures/external.scenario.json --journal external/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-transcript external/run.strategy.jsonl --strategy-timeout 5 --durable-artifacts
  run=external-demo audits=12 orders=1 active=0 filled=1 rejected=0
  cash=9793.544 equity=10007.544 gross=214 realized=0 unrealized=7.544 fees=0.456
  journal=external/run.journal.jsonl
  strategy_transcript=external/run.strategy.jsonl

  $ python3 -c 'from pathlib import Path; print(len(Path("external/run.journal.jsonl").read_text().splitlines()), len(Path("external/run.strategy.jsonl").read_text().splitlines()))'
  12 14
  $ diff -u ../contracts/strategy/v6/fixtures/external.strategy.jsonl external/run.strategy.jsonl

  $ mkdir callback-ordering
  $ ../bin/main.exe --input ../contracts/strategy/v6/fixtures/external.scenario.json --journal callback-ordering/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-arg cancel-next --strategy-transcript callback-ordering/run.strategy.jsonl --strategy-timeout 5
  run=external-demo audits=12 orders=2 active=0 filled=1 rejected=0
  cash=9896.647 equity=10003.647 gross=107 realized=0 unrealized=3.647 fees=0.353
  journal=callback-ordering/run.journal.jsonl
  strategy_transcript=callback-ordering/run.strategy.jsonl
  $ python3 -c 'import json; journal=[json.loads(line) for line in open("callback-ordering/run.journal.jsonl")]; transcript=[json.loads(line) for line in open("callback-ordering/run.strategy.jsonl")]; events=[record["event_type"] for record in journal]; cancellations=[record["payload"]["reason"] for record in journal if record["event_type"] == "order_cancelled"]; requests=[record["message"]["payload"] for record in transcript if record["direction"] == "engine_to_strategy" and record["message"]["message_type"] == "event"]; fill=next(request for request in requests if request["event"]["type"] == "fill_received"); following=requests[requests.index(fill) + 1]; print(events.count("fill_applied"), cancellations, events.count("intent_rejected")); print(len(fill["context"]["working_orders"]), len(following["context"]["working_orders"])); print(fill["context"]["latest_bars"][0]["close"], fill["context"]["portfolio"]["positions"][0]["mark"])'
  1 ['strategy_requested'] 0
  1 0
  107 107

  $ mkdir failed-external
  $ ../bin/main.exe --input ../contracts/strategy/v6/fixtures/external.scenario.json --journal failed-external/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-arg stall --strategy-transcript failed-external/run.strategy.jsonl --strategy-timeout 0.01
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
  >   output=$(../bin/main.exe --input ../contracts/strategy/v6/fixtures/external.scenario.json --journal "$directory/run.journal.jsonl" --strategy-executable ./fake_strategy.py --strategy-arg "$mode" --strategy-transcript "$directory/run.strategy.jsonl" --strategy-timeout 5 2>&1)
  >   status=$?
  >   test "$status" -eq 123 || return 1
  >   case "$output" in *"$expected"*) ;; *) return 1 ;; esac
  >   test ! -e "$directory/run.journal.jsonl" || return 1
  >   test -e "$directory/run.journal.jsonl.partial" || return 1
  >   test ! -e "$directory/run.strategy.jsonl" || return 1
  >   test -e "$directory/run.strategy.jsonl.partial" || return 1
  >   if test "$#" -eq 3; then
  >     python3 - "$directory/run.strategy.jsonl.partial" "$mode" "$3" <<'PY' || return 1
  > import json
  > import sys
  > from pathlib import Path
  > path, mode, expected_code = sys.argv[1:]
  > records = [json.loads(line) for line in Path(path).read_text().splitlines()]
  > rejection = records[-1]
  > assert rejection["strategy_diagnostic_version"] == "1"
  > assert rejection["record_type"] == "rejected_strategy_response"
  > assert rejection["expected_strategy_sequence"] == "1"
  > assert rejection["diagnostic"]["code"] == expected_code
  > assert rejection["diagnostic"]["context"]["sequence"] == "1"
  > assert "strategy_protocol_version" not in rejection
  > assert "direction" not in rejection
  > assert "message" not in rejection
  > evidence = rejection["evidence"]
  > assert evidence["encoding"] == "hex"
  > raw = bytes.fromhex(evidence["prefix"])
  > assert len(raw) <= 256
  > if mode == "eof":
  >     assert raw == b"" and evidence["observed_bytes"] == 0
  >     assert evidence["truncated"] is False
  > elif mode == "oversized":
  >     assert raw == b"x" * 256
  >     assert evidence["observed_bytes"] == 1_048_577
  >     assert evidence["truncated"] is True
  > elif mode == "malformed":
  >     assert raw == b"{" and evidence["observed_bytes"] == 1
  >     assert evidence["truncated"] is False
  > else:
  >     assert evidence["observed_bytes"] == len(raw)
  >     assert evidence["truncated"] is False
  >     response = json.loads(raw)
  >     if mode == "bad-sequence":
  >         assert response["strategy_sequence"] == "999"
  >     elif mode == "wrong-version":
  >         assert response["strategy_protocol_version"] == "1"
  >     elif mode == "unknown-field":
  >         assert response["unexpected"] is True
  > PY
  >   fi
  >   echo "$mode: rejected"
  > }
  $ check_strategy_failure eof "closed stdout" strategy.protocol
  eof: rejected
  $ check_strategy_failure bad-sequence "expected strategy sequence" strategy.protocol
  bad-sequence: rejected
  $ check_strategy_failure error "fixture failure"
  error: rejected
  $ check_strategy_failure extra-output "data after its stopped response"
  extra-output: rejected
  $ check_strategy_failure nonzero "exited with code 9"
  nonzero: rejected
  $ check_strategy_failure malformed "invalid strategy response JSON" strategy.protocol
  malformed: rejected
  $ check_strategy_failure oversized "exceeds the maximum message size" resource.limit
  oversized: rejected
  $ check_strategy_failure wrong-version "unsupported strategy protocol version" strategy.protocol
  wrong-version: rejected
  $ check_strategy_failure unknown-field "unknown or missing fields" strategy.protocol
  unknown-field: rejected

  $ check_process_tree_failure () {
  >   mode="$1"
  >   expected="$2"
  >   directory="process-tree-$mode"
  >   mkdir "$directory"
  >   pid_path="$directory/grandchild.pid"
  >   output=$(../bin/main.exe --input ../contracts/strategy/v6/fixtures/external.scenario.json --journal "$directory/run.journal.jsonl" --strategy-executable ./fake_strategy.py --strategy-arg "$mode" --strategy-arg "$pid_path" --strategy-transcript "$directory/run.strategy.jsonl" --strategy-timeout 0.2 2>&1)
  >   status=$?
  >   test "$status" -eq 123 || return 1
  >   case "$output" in *"$expected"*) ;; *) return 1 ;; esac
  >   test -s "$pid_path" || return 1
  >   pid=$(cat "$pid_path")
  >   python3 - "$pid" <<'PY' || return 1
  > import os
  > import sys
  > import time
  > pid = int(sys.argv[1])
  > deadline = time.monotonic() + 2
  > while True:
  >     try:
  >         os.kill(pid, 0)
  >     except ProcessLookupError:
  >         break
  >     if time.monotonic() >= deadline:
  >         raise SystemExit("grandchild process survived cleanup")
  >     time.sleep(0.01)
  > PY
  >   echo "$mode: process tree reaped"
  > }
  $ check_process_tree_failure spawn-grandchild "timed out"
  spawn-grandchild: process tree reaped
  $ check_process_tree_failure grandchild-malformed "invalid strategy response JSON"
  grandchild-malformed: process tree reaped

  $ mkdir external-stream
  $ ../bin/main.exe --input-format jsonl --input ../contracts/strategy/v6/fixtures/external.scenario.jsonl --journal external-stream/run.journal.jsonl --strategy-executable ./fake_strategy.py --strategy-transcript external-stream/run.strategy.jsonl --strategy-timeout 5
  run=external-demo audits=12 orders=1 active=0 filled=1 rejected=0
  cash=9793.544 equity=10007.544 gross=214 realized=0 unrealized=7.544 fees=0.456
  journal=external-stream/run.journal.jsonl
  strategy_transcript=external-stream/run.strategy.jsonl
