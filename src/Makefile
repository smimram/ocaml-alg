SOURCES = extlib.ml utils.ml \
          alphabet.ml monoid.ml group.ml ring.ml field.ml matrix.ml module.ml algebra.ml series.ml \
          hypergraph.ml graph.ml globular.ml simplicial.ml category.ml precategory.ml \
          automaton.ml term.ml
RESULT = alg
INCDIRS =
LIBS =
ANNOTATE = true
USE_CAMLP4 =
DOC_FILES = $(SOURCES)
OCAMLDEP = ocamldep.opt
OCAMLOPT = ocamlopt.opt
OCAMLDOC = ocamldoc.opt
OCAMLDOCFLAGS = -sort

all: dncl

test: all
	$(OCAMLOPT) -bin-annot -I .. alg.cmxa test.ml -o test
	OCAMLRUNPARAM=b ./test

include OCamlMakefile
