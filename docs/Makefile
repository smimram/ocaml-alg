DOCS_DIR := $(shell pwd)
BERGMAN_DIR = ../tools/bergman
BERGMAN_FILES = index.html bergman.js bergman.css
KB_DIR = ../tools/kb
KB_FILES = index.html kb.js kb.css

all: alg bergman kb

alg:
	$(MAKE) -C ../src htdoc
	cp ../src/doc/alg/html/* alg/
	git add alg/*.html alg/*.css

bergman:
	$(MAKE) -C $(BERGMAN_DIR)
	cd $(BERGMAN_DIR); cp $(BERGMAN_FILES) $(DOCS_DIR)/bergman
	cd $(DOCS_DIR)/bergman; git add $(BERGMAN_FILES)

kb:
	$(MAKE) -C $(KB_DIR)
	cd $(KB_DIR); cp $(KB_FILES) $(DOCS_DIR)/kb
	cd $(DOCS_DIR)/kb; git add $(KB_FILES)

ci: all
	git ci . -m "Update documentation."

.PHONY: alg bergman kb
