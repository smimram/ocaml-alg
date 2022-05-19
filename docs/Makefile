DOCS_DIR := $(shell pwd)

all: alg bergman kb index.html

doc build:
	$(MAKE) -C .. $@

clean:
	rm -rf alg bergman kb

alg: doc
	mkdir -p $@
	cp -r ../_build/default/_doc/_html/* alg/

bergman: build
	mkdir -p $@
	cd ../_build/default/tools/bergman; mv bergman.bc.js bergman.js; cp index.html bergman.css bergman.js $(DOCS_DIR)/bergman

kb: build
	mkdir -p $@
	cd ../_build/default/tools/kb; mv kb.bc.js kb.js; cp index.html kb.css kb.js $(DOCS_DIR)/kb

ci: all
	git ci . -m "Update documentation."

%.html: %.md
	pandoc -s $< -o $@

.PHONY: alg bergman kb
