all clean:
	$(MAKE) -C src $@
	$(MAKE) -C tools $@

test:
	$(MAKE) -C test

.PHONY: test
