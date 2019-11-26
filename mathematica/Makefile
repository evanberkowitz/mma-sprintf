USER_PACKAGES=${HOME}/Library/Mathematica/Applications

HERE=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))
PACKAGES=$(shell find . -type f -name '*.wl' | sed 's/^..//g' )

LINKS=$(foreach p,$(PACKAGES),$(USER_PACKAGES)/$(p))

install: $(LINKS)

$(USER_PACKAGES)/%.wl:
	ln -s $(HERE)$*.wl $@

$(USER_PACKAGES)/%.m:
	ln -s $(HERE)$*.m $@

uninstall:
	rm $(LINKS)
