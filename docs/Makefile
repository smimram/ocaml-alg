DOCS_DIR := $(shell pwd)
BERGMAN_DIR = ../tools/bergman
BERGMAN_FILES = index.html bergman.js bergman.css

all: alg bergman

alg:
	$(MAKE) -C ../src htdoc
	cp ../src/doc/alg/html/* alg/
	git add alg/*.html alg/*.css

bergman:
	$(MAKE) -C $(BERGMAN_DIR)
	cd $(BERGMAN_DIR); cp $(BERGMAN_FILES) $(DOCS_DIR)/bergman
	cd $(DOCS_DIR)/bergman; git add $(BERGMAN_FILES)

.PHONY: alg bergman
