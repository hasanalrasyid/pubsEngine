#!/bin/bash

main=$1

# Compile document
lualatex -interaction=nonstopmode ${main}

# Compile bibliography
bibtex ${main}

# Compile document
lualatex -interaction=nonstopmode ${main}

# Compile document
lualatex -interaction=nonstopmode ${main}
