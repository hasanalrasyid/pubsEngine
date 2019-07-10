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
  cd extern/nodejs
  yarn add mermaid
  yarn add mermaid.cli
  ln -s /home/cns/Documents/marleni/pubsEngine/diagrams-pandoc/extern/nodejs/node_modules/.bin/mmdc /home/cns/.local/bin/mmdc
  ln -s /home/cns/.local/bin/mmdc /home/cns/.local/bin/mermaid
