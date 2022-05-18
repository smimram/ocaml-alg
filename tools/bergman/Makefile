SOURCES = pol.ml parser.mly lexer.mll interface.ml
RESULT = bergman
PACKS = js_of_ocaml js_of_ocaml-ppx
LIBS = alg
ALGDIR = ../../src
INCDIRS = $(ALGDIR)
ANNOTATE = true

all: alg dc
	js_of_ocaml compile $(RESULT)

alg:
	@$(MAKE) -C $(ALGDIR) dcl INCDIRS=

test: all test.ml
	$(OCAMLC) -g pol.cmo parser.cmo lexer.cmo test.ml -o test
	./test

conflicts:
	menhir --explain parser.mly && less parser.conflicts && rm parser.ml parser.mli parser.conflicts

.PHONY: alg

include OCamlMakefile
