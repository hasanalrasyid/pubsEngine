#!/bin/bash

# Compile document
lualatex -interaction=nonstopmode main

# Compile nomenclature
makeindex main.nlo -s nomencl.ist -o main.nls

# Compile index
makeindex main

# Compile bibliography
biber main

# Compile document
lualatex -interaction=nonstopmode main

# Compile glossary
makeglossaries main

# Compile document
lualatex -interaction=nonstopmode main
