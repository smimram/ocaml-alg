OCAMLOPT = ocamlopt

test: test.ml
	$(MAKE) -C ../../src
	$(OCAMLOPT) -bin-annot -I ../../src alg.cmxa test.ml -o test
	OCAMLRUNPARAM=b ./test
