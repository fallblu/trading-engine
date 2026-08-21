#!/usr/bin/env python3
"""Deterministic strategy-protocol fixture used by the CLI test."""

from __future__ import annotations

import json
import os
import signal
import subprocess
import sys
import time


MODE = sys.argv[1] if len(sys.argv) > 1 else "success"


if MODE in {
    "spawn-grandchild",
    "spawn-grandchild-success",
    "grandchild-malformed",
}:
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    grandchild_pid_path = sys.argv[2]
    grandchild = """
import os
import signal
import sys
import time

signal.signal(signal.SIGTERM, signal.SIG_IGN)
with open(sys.argv[1], "w", encoding="ascii") as channel:
    channel.write(str(os.getpid()))
    channel.flush()
time.sleep(60)
"""
    subprocess.Popen(
        [sys.executable, "-c", grandchild, grandchild_pid_path],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        close_fds=True,
    )
    deadline = time.monotonic() + 1
    while not os.path.exists(grandchild_pid_path):
        if time.monotonic() >= deadline:
            raise RuntimeError("grandchild did not publish its PID")
        time.sleep(0.01)


def response(request: dict[str, object]) -> dict[str, object]:
    sequence = request["strategy_sequence"]
    message_type = request["message_type"]
    if message_type == "initialize":
        response_type = "ready"
        payload: dict[str, object] = {
            "strategy_name": "fixture-strategy",
            "strategy_version": "1",
        }
    elif message_type == "event":
        response_type = "intents"
        request_payload = request["payload"]
        assert isinstance(request_payload, dict)
        event = request_payload["event"]
        assert isinstance(event, dict)
        market_slice = event.get("market_slice")
        if (
            event["type"] == "market_slice_closed"
            and isinstance(market_slice, dict)
            and market_slice["slice_sequence"] == "1"
        ):
            if MODE == "cancel-next":
                order = {
                    "type": "submit_order",
                    "instrument_id": "demo-equity-acme",
                    "side": "buy",
                    "quantity": "1",
                    "order_kind": "market",
                    "limit_price": None,
                }
                payload = {"intents": [order, order]}
            else:
                payload = {
                    "intents": [
                        {
                            "type": "target_quantities",
                            "targets": [
                                {
                                    "instrument_id": "demo-equity-acme",
                                    "quantity": "2",
                                }
                            ],
                        },
                        {
                            "type": "emit_metric",
                            "name": "fixture_signal",
                            "value": "2",
                        },
                    ]
                }
        elif MODE == "cancel-next" and event["type"] == "fill_received":
            context = request_payload["context"]
            assert isinstance(context, dict)
            working_orders = context["working_orders"]
            assert isinstance(working_orders, list) and len(working_orders) == 1
            remaining_order = working_orders[0]
            assert isinstance(remaining_order, dict)
            payload = {
                "intents": [
                    {
                        "type": "cancel_order",
                        "order_id": remaining_order["order_id"],
                    }
                ]
            }
        else:
            payload = {"intents": []}
    elif message_type == "shutdown":
        response_type = "stopped"
        payload = {}
    else:
        response_type = "error"
        payload = {"message": "unsupported request"}
    return {
        "strategy_protocol_version": "3",
        "strategy_sequence": sequence,
        "message_type": response_type,
        "payload": payload,
    }


for line in sys.stdin:
    request = json.loads(line)
    if MODE == "eof":
        raise SystemExit(0)
    if MODE == "stall":
        time.sleep(60)
    if MODE == "spawn-grandchild":
        time.sleep(60)
    if MODE in {"malformed", "grandchild-malformed"}:
        print("{", flush=True)
        continue
    if MODE == "oversized":
        print("x" * 1_048_577, flush=True)
        continue
    message = response(request)
    if MODE == "bad-sequence":
        message["strategy_sequence"] = "999"
    if MODE == "wrong-version":
        message["strategy_protocol_version"] = "1"
    if MODE == "unknown-field":
        message["unexpected"] = True
    if MODE == "error" and request["message_type"] == "event":
        message["message_type"] = "error"
        message["payload"] = {"message": "fixture failure"}
    print(json.dumps(message, separators=(",", ":")), flush=True)
    if request["message_type"] == "shutdown":
        if MODE == "extra-output":
            print("{}", flush=True)
        if MODE == "nonzero":
            raise SystemExit(9)
        break
