needed install in centos 7:
  gmp-devel
  gmp-static
  zlib-static
  ncurses-static

  install pkg-listOutside dari luar

to enable mermaid, in centos 7, do this:
remove stock npm (6._)
  yum remove npm nodejs

install nodejs 12._ :
as root
  curl --silent --location https://rpm.nodesource.com/setup_12.x | bash -

install yarn:
as user:
  curl -o- -L https://yarnpkg.com/install.sh | bash

install mermaid:
  npm install @mermaid-js/mermaid-cli
  ln -sf  ~/node_modules/.bin/mmdc ~/.local/bin/mermaid
