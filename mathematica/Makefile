INSTALL := $(shell wolframscript -code '$$UserBaseDirectory')
ORIGINAL := $(shell wolframscript -code 'Directory[]')

PACKAGES := $(shell find . -type f -name '*.wl' | sed -e 's:./::g')

LINKS=$(foreach p, $(PACKAGES), $(INSTALL)/Applications/$(p))

install: $(LINKS)

$(INSTALL)/%.wl:
	ln -s $(ORIGINAL)$*.wl $@

$(INSTALL)/%.m:
	ln -s $(ORIGINAL)$*.m $@

uninstall:
	rm $(LINKS)
