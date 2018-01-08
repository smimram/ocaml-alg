SOURCES = interface.ml
RESULT = bergman
PACKS = js_of_ocaml,js_of_ocaml.syntax
OCAMLFLAGS = -syntax camlp4o
ALGDIR = ../../src

all: alg
	ocamlfind ocamlc -annot -I $(ALGDIR) alg.cma -package $(PACKS) -syntax camlp4o -linkpkg -o $(RESULT) $(SOURCES)
	js_of_ocaml $(RESULT)

test: all test.ml
	$(OCAMLC) -g -pp "camlp4o" test.ml -o test
	OCAMLRUNPARAM=b ./test

alg:
	@$(MAKE) -C $(ALGDIR) dcl INCDIRS=

.PHONY: alg
