#!/bin/bash

main=$1
if [ -z "$1" ]; then
  echo Manuscript name is needed
  echo compileall.sh manuscript
  exit
fi

# Compile document
lualatex -interaction=nonstopmode ${main}

# Compile nomenclature
makeindex ${main}.nlo -s nomencl.ist -o ${main}.nls

# Compile index
makeindex ${main}

# Compile bibliography
biber ${main}

# Compile document
lualatex -interaction=nonstopmode ${main}

# Compile glossary
makeglossaries ${main}

# Compile document
lualatex -interaction=nonstopmode ${main}
