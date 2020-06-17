OCAMLOPT = ocamlopt

all: groups

%: %.ml
	$(MAKE) -C ../../src
	$(OCAMLOPT) -bin-annot -I ../../src alg.cmxa $< -o $@
	OCAMLRUNPARAM=b ./$@
