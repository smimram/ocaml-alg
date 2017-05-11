all clean:
	$(MAKE) -C src $@
	$(MAKE) -C tools $@

doc:
	$(MAKE) -C src htdoc
