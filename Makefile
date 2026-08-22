export PATH := $(CURDIR)/.venv-schema/bin:$(PATH)

.PHONY: bootstrap environment-check build test fmt-check check

bootstrap:
	@./scripts/bootstrap-development-environment

environment-check:
	@./scripts/check-development-environment

build: environment-check
	opam exec -- dune build @all

test: environment-check
	opam exec -- dune runtest

fmt-check: environment-check
	opam exec -- dune build @fmt

check: fmt-check build test
