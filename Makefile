INSTALL := $(shell wolframscript -code '$$UserBaseDirectory')
ORIGINAL := $(shell wolframscript -code 'Directory[]')

APPS := $(shell find Applications -type f)

LINKS=$(foreach p, $(APPS), $(INSTALL)/$(p))

install: $(LINKS)

$(INSTALL)/%.wl:
	ln -s $(ORIGINAL)/$*.wl $@

$(INSTALL)/%.m:
	ln -s $(ORIGINAL)/$*.m $@

uninstall:
	$(RM) $(LINKS)
