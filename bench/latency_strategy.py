#!/usr/bin/env python3
"""No-op strategy that adds deterministic per-event response latency."""

from __future__ import annotations

import json
import sys
import time


LATENCY_SECONDS = float(sys.argv[1]) / 1000 if len(sys.argv) > 1 else 0.0
if LATENCY_SECONDS < 0:
    raise ValueError("latency must not be negative")


for line in sys.stdin:
    request = json.loads(line)
    message_type = request["message_type"]
    if message_type == "initialize":
        response_type = "ready"
        payload = {
            "strategy_name": "benchmark-latency",
            "strategy_version": "1",
        }
    elif message_type == "event":
        if LATENCY_SECONDS:
            time.sleep(LATENCY_SECONDS)
        response_type = "intents"
        payload = {"intents": []}
    elif message_type == "shutdown":
        response_type = "stopped"
        payload = {}
    else:
        response_type = "error"
        payload = {"message": "unsupported request"}
    print(
        json.dumps(
            {
                "strategy_protocol_version": "3",
                "strategy_sequence": request["strategy_sequence"],
                "message_type": response_type,
                "payload": payload,
            },
            separators=(",", ":"),
        ),
        flush=True,
    )
    if message_type == "shutdown":
        break
