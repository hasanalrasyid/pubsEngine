BUILDPATH:=$(shell find ./.stack-work/install|grep pkgdb$$|sed -e 's/^.*install.//g' -e 's/.pkgdb//g')
#EXTs := $(shell ls extern/*tbz)
Extracted := $(lastword $(shell find extern -maxdepth 1 -type d))

installfast:
	stack build pubsEngine:pubsEngine

install:
	./preMake.sh aas
	./preMake.sh snat
	./preMake.sh thesis
	./preMake.sh revealjs
	./preMake.sh plain
	./preMake.sh book
	teckit_compile templates/common/fontmap/nusantara-trans-novoc.map
	stack build pubsEngine:pubsEngine
##tar -cjf pubsEngine.tbz /home/aku/Documents/kerja/devKit/pubsEngine/.stack-work/install/x86_64-linux-tinfo6/140824c1d005ba1b38413892db457f57b054423f097e02376f2d07b509752b33/8.10.7/bin/pubsEngine
##scp pubsEngine.tbz gitmki:~/

send:
	md5sum .stack-work/install/${BUILDPATH}/bin/pubsEngine
	socat -u FILE:.stack-work/install/${BUILDPATH}/bin/pubsEngine TCP-LISTEN:3000,reuseaddr

installdeep:
	mkdir -p $(HOME)/.local/bin
	rm -f $(HOME)/.local/bin/pubsEngine
	ln -f -s $(PWD)/.stack-work/install/${BUILDPATH}/bin/pubsEngine $(HOME)/.local/bin/
	echo runit using : 'pandoc -t latex --filter thesis test/test1.md -s'
