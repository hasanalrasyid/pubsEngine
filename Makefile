BUILDPATH:=$(shell find ./.stack-work/install|grep pkgdb$$|sed -e 's/^.*install.//g' -e 's/.pkgdb//g')
EXTs := $(shell ls extern/*tbz)
Extracted := $(lastword $(shell find extern -maxdepth 1 -type d))

install:
	./preMake.sh article
	teckit_compile templates/common/fontmap/nusantara-trans-novoc.map
	stack build pubsEngine:pubsEngine

installdeep:
	mkdir -p $(HOME)/.local/bin
	rm -f $(HOME)/.local/bin/pubsEngine
	ln -f -s $(PWD)/.stack-work/install/${BUILDPATH}/bin/pubsEngine $(HOME)/.local/bin/
	echo runit using : 'pandoc -t latex --filter thesis test/test1.md -s'
