export PATH := $(CURDIR)/.venv-schema/bin:$(PATH)

.PHONY: bootstrap environment-check build test fmt-check check

bootstrap:
	@./scripts/bootstrap-development-environment

environment-check:
	@./scripts/check-development-environment

build:
	opam exec -- dune build @all

test:
	opam exec -- dune runtest

fmt-check:
	opam exec -- dune build @fmt

check: environment-check fmt-check build test
