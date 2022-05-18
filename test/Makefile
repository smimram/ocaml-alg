ML = $(wildcard *.ml)
PROG = $(ML:.ml=)

all: .gitignore test

test: build
	@for p in $(PROG); do echo "\n\n##### Running $$p #####\n"; ./$$p || exit 1; done

build: alg $(PROG)

alg:
	@$(MAKE) -C ../src

clean:
	rm -f $(PROG) *.cm* *.o

pdf: build
	./squierGroup
	pdflatex squierGroupFull.tex

%: %.ml
	ocamlopt.opt -annot -I ../src alg.cmxa $< -o $@

.gitignore: $(ML)
	rm -f .gitignore
	for p in $(PROG); do echo "$$p" >> $@; done
	echo "squierGroup.tex" >> $@

.PHONY: alg
