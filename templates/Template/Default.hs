{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Template.Default where

import Text.RawString.QQ
import qualified Data.Text as T

templateYaml :: T.Text
templateYaml = [r|
---
documentclass: book
fontsize: 11pt
colorlinks: true
lang: en-UK
polyglossia-lang:
  name: english
  options: variant=uk
natbib: true
toc: true
lot: true
lof: true
natbiboptions: |
            square,
            numbers,
            comma
bibliography: ""
biblio-style: unsrtnat
biblio-title: Reference
papersize: a4
header-includes: |
  \usepackage{appendix}
  \usepackage{chemfig}
  \usepackage[version=4]{mhchem}
  \mhchemoptions{layout=stacked}
  \usepackage{longtable}
  \usepackage{siunitx}
  \usepackage{booktabs}
include-before: |
  \input{hasanHyp}
  \sisetup{table-format=-3.4}
title: "Default Title"
author: "Default Author Ph.D"
email: "defaultEmail@domain.edu"
abstract: "Default Abstract"
appendix:
linkDir:
---
  |]
