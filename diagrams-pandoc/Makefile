EXTs := $(shell ls extern/*tbz)
Extracted := $(lastword $(shell find extern -maxdepth 1 -type d))

justbuild:
	stack build diagrams-pandoc:pubsEngine

build:
ifeq ($(Extracted),extern)
	$(info "Unpacking external libraries")
	$(foreach tbz,$(EXTs),\
		$(shell tar -xf $(tbz) -C extern))
else
	$(info "Checked external libraries")
endif
	stack install pandoc
	stack build

install:
	mkdir -p $(HOME)/.local/bin
	ln -f -s $(PWD)/bin/* $(HOME)/.local/bin/
	echo runit using : 'pandoc -t latex --filter thesis test/test1.md -s'
