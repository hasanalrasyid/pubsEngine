EXTs := $(shell ls extern/*tbz)
Extracted := $(lastword $(shell find extern -maxdepth 1 -type d))

build:
ifeq ($(Extracted),extern)
	$(info "Checked external libraries")
else
	$(info "Unpacking external libraries")
	$(foreach tbz,$(EXTs),\
		$(shell tar -xf $(tbz) -C extern))
endif
	stack install pandoc
	stack build

install:
	stack build pubsEngine:pubsEngine

installdeep:
	mkdir -p $(HOME)/.local/bin
	rm -f $(HOME)/.local/bin/pubsEngine
	ln -f -s $(PWD)/.stack-work/install/x86_64-linux-tinfo6/34569879eae35f9678d9b28316e35482b41e5f560814f8d6159a51cf03d39aea/8.2.2/bin/pubsEngine $(HOME)/.local/bin/
	echo runit using : 'pandoc -t latex --filter thesis test/test1.md -s'