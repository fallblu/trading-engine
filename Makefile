.PHONY: build test fmt-check check

build:
	opam exec -- dune build @all

test:
	opam exec -- dune runtest

fmt-check:
	opam exec -- dune build @fmt

check: fmt-check build test
