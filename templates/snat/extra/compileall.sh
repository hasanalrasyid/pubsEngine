#!/bin/bash

main=$1
if [ -z "$1" ]; then
  echo Manuscript name is needed
  echo compileall.sh manuscript
  exit
fi

# Compile document
lualatex -interaction=nonstopmode ${main}

# Compile bibliography
bibtex ${main}

# Compile document
lualatex -interaction=nonstopmode ${main}

# Compile document
lualatex -interaction=nonstopmode ${main}
