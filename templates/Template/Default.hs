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
  \providecommand{\tightlist}{%
  \setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
include-before: |
  \input{hasanHyp}
  \sisetup{table-format=-3.4}
title:
author:
titleshort:
authorshort:
email: "defaultEmail@domain.edu"
abstract:
abstractTex:
appendix:
linkDir:
acknowledgements:
keywords: default keywords -- math symbol $\gamma$ -- latex -- pandoc

---
  |]
