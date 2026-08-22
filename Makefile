export PATH := $(CURDIR)/.venv-schema/bin:$(PATH)

.PHONY: bootstrap environment-check build test metadata-check docs-bootstrap docs-source-check docs-check docs-build determinism-check dependency-band-check coverage benchmark-smoke benchmark fuzz-smoke fuzz fmt-check check

FUZZ_SEED ?= 20260821
FUZZ_CASES ?= 10000
BENCHMARK_OUTPUT ?= benchmark-results/replay.json

bootstrap:
	@./scripts/bootstrap-development-environment

environment-check:
	@./scripts/check-development-environment

build:
	opam exec -- dune build @all

test:
	opam exec -- dune runtest

metadata-check:
	python3 test/test_repository_metadata.py

docs-bootstrap:
	@./scripts/bootstrap-documentation-environment

docs-source-check:
	python3 scripts/check-documentation.py source

docs-check: docs-source-check

docs-build: docs-bootstrap docs-check
	@./scripts/build-documentation-site

determinism-check: build
	@./scripts/check-deterministic-journals

dependency-band-check: fmt-check build test metadata-check determinism-check benchmark-smoke

coverage: environment-check
	@./scripts/check-ocaml-coverage

benchmark-smoke: build
	python3 bench/benchmark_replay.py --suite smoke --repetitions 1 --warmups 0

benchmark: build
	python3 bench/benchmark_replay.py --output $(BENCHMARK_OUTPUT)

fuzz-smoke:
	opam exec -- dune exec test/fuzz_protocol.exe -- --seed 20260821 --cases 256

fuzz:
	opam exec -- dune exec test/fuzz_protocol.exe -- --seed $(FUZZ_SEED) --cases $(FUZZ_CASES)

fmt-check:
	opam exec -- dune build @fmt

check: environment-check fmt-check build test metadata-check docs-source-check determinism-check benchmark-smoke
