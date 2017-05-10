SOURCES = alphabet.ml monoid.ml group.ml ring.ml field.ml matrix.ml module.ml algebra.ml series.ml \
	globular.ml category.ml precategory.ml \
	automaton.ml
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
	$(OCAMLOPT) -annot -I .. alg.cmxa test3.ml -o test
	OCAMLRUNPARAM=b ./test

include OCamlMakefile
