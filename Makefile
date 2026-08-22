export PATH := $(CURDIR)/.venv-schema/bin:$(PATH)

.PHONY: bootstrap environment-check build test fuzz-smoke fuzz fmt-check check

FUZZ_SEED ?= 20260821
FUZZ_CASES ?= 10000

bootstrap:
	@./scripts/bootstrap-development-environment

environment-check:
	@./scripts/check-development-environment

build:
	opam exec -- dune build @all

test:
	opam exec -- dune runtest

fuzz-smoke:
	opam exec -- dune exec test/fuzz_protocol.exe -- --seed 20260821 --cases 256

fuzz:
	opam exec -- dune exec test/fuzz_protocol.exe -- --seed $(FUZZ_SEED) --cases $(FUZZ_CASES)

fmt-check:
	opam exec -- dune build @fmt

check: environment-check fmt-check build test
