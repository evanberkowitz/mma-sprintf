
all:
	cd mathematica; $(MAKE)
	
uninstall:
	cd mathematica; $(MAKE) uninstall
	
.PHONY: all uninstall