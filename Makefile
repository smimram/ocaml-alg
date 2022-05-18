all: build

build:
	@dune build

doc:
	@dune build @doc

clean:
	@dune clean

test:
	@dune runtest

.PHONY: test
