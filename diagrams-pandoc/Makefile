build:
	stack install pandoc
	stack build
	mkdir -p bin
	cd bin && ln -f -s ../.stack-work/install/x86_64-linux/lts-11.9/8.2.2/bin/* .

install:
	mkdir -p $(HOME)/.local/bin
	ln -f -s $(PWD)/bin/* $(HOME)/.local/bin/
	echo runit using : 'pandoc -t latex --filter diagrams-pandoc.hs test/test1.md -s'
