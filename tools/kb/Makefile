SOURCES = parserRefs.ml parser.mly lexer.mll interface.ml
RESULT = kb
PACKS = js_of_ocaml js_of_ocaml-ppx
LIBS = alg
ALGDIR = ../../src
INCDIRS = $(ALGDIR)
ANNOTATE = true

all: alg dc
	js_of_ocaml $(RESULT)

alg:
	@$(MAKE) -C $(ALGDIR) dcl INCDIRS=

test: all test.ml
	$(OCAMLC) -I $(ALGDIR) alg.cma -g parserRefs.cmo parser.cmo lexer.cmo test.ml -o test
	./test

conflicts:
	menhir --explain parser.mly && less parser.conflicts && rm parser.ml parser.mli parser.conflicts

.PHONY: alg

include OCamlMakefile
