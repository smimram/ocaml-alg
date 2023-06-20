DOCS_DIR := $(shell pwd)

all: alg bergman kb rewr2 index.html

doc build:
	$(MAKE) -C .. $@

clean:
	rm -rf alg bergman kb

alg: doc
	mkdir -p $@
	cp -r ../_build/default/_doc/_html/* alg/
	chmod -R +w alg/*

bergman: build
	mkdir -p $@
	cd ../_build/default/tools/bergman; mv bergmanjs.bc.js bergman.js; cp index.html bergman.css bergman.js $(DOCS_DIR)/bergman

kb: build
	mkdir -p $@
	cd ../_build/default/tools/kb; mv kb.bc.js kb.js; cp index.html kb.css kb.js $(DOCS_DIR)/kb

rewr2: build
	mkdir -p $@
	cd ../_build/default/tools/rewr2; mv rewr2.bc.js rewr2.js; cp index.html rewr2.js $(DOCS_DIR)/rewr2

ci: all
	git ci . -m "Update documentation."

%.html: %.md
	pandoc -s $< -o $@

.PHONY: alg bergman kb
