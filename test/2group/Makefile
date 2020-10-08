OCAMLOPT = ocamlopt

all: groups2

%: %.ml
	$(MAKE) -C ../../src
	$(OCAMLOPT) -bin-annot -I ../../src alg.cmxa $< -o $@
	OCAMLRUNPARAM=b ./$@
