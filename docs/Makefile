DOCS_DIR := $(shell pwd)

all: alg bergman kb

doc build:
	$(MAKE) -C .. $@

alg: doc
	rm -rf alg/alg
	chmod +w alg/*
	cp -r ../_build/default/_doc/_html/* alg/
	cd alg; git add *.js *.css *.html alg

bergman: build
	cd ../_build/default/tools/bergman; mv bergman.bc.js bergman.js; cp index.html bergman.css bergman.js $(DOCS_DIR)/bergman
	cd bergman; git add *

kb: build
	cd ../_build/default/tools/kb; mv kb.bc.js kb.js; cp index.html kb.css kb.js $(DOCS_DIR)/kb
	cd kb; git add *

ci: all
	git ci . -m "Update documentation."

.PHONY: alg bergman kb
