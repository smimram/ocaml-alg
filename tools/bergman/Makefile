SOURCES = interface.ml bergman.ml
RESULT = bergman
INCDIRS = $(ALGDIR) +../js_of_ocaml
LIBS = alg js_of_ocaml
ANNOTATE = true
USE_CAMLP4 = true
ALGDIR = ../../src

all: alg dc
	js_of_ocaml $(RESULT)

test: all test.ml
	$(OCAMLC) -g -pp "camlp4o" test.ml -o test
	OCAMLRUNPARAM=b ./test

alg:
	@$(MAKE) -C $(ALGDIR) dcl INCDIRS=

include OCamlMakefile

.PHONY: alg
