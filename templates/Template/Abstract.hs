{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Template.Abstract where

import Text.RawString.QQ

templateLatex :: IO String
templateLatex = return [r|
%%%% Template Abstract
% Options for packages loaded elsewhere
\PassOptionsToPackage{unicode$for(hyperrefoptions)$,$hyperrefoptions$$endfor$}{hyperref}
\PassOptionsToPackage{hyphens}{url}
$if(colorlinks)$
\PassOptionsToPackage{dvipsnames,svgnames*,x11names*}{xcolor}
$endif$
$if(dir)$
$if(latex-dir-rtl)$
\PassOptionsToPackage{RTLdocument}{bidi}
$endif$
$endif$
%
\documentclass{article}

\usepackage[numbers]{natbib}
\usepackage{rotating}
\usepackage{graphicx}

\usepackage[$margin$]{geometry}

$if(csl-refs)$
\newlength{\cslhangindent}
\setlength{\cslhangindent}{1.5em}
\newenvironment{cslreferences}%
  {$if(csl-hanging-indent)$\setlength{\parindent}{0pt}%
  \everypar{\setlength{\hangindent}{\cslhangindent}}\ignorespaces$endif$}%
  {\par}
$endif$

$for(header-includes)$
$header-includes$
$endfor$

% Document starts
\begin{document}
$for(include-before)$
$include-before$
$endfor$


% Title portion
$if(title)$
$title$
$endif$

%\affil[aff1]{Replace this text with an author's affiliation (use complete addresses). Note the use of superscript ``a)'' to indicate the author's e-mail address below. Use b), c), etc. to indicate e-mail addresses for more than 1 author.}
%\affil[aff2]{Additional affiliations should be indicated by superscript numbers 2, 3, etc. as shown above.}
%\affil[aff3]{You would list an author's second affiliation here.}
%\corresp[cor1]{Corresponding author: your@emailaddress.xxx}

$if(author)$
$for(author)$$if(author.newline)$\\ $endif$$if(author.correspond)$\underline{$endif$$author.name$$if(author.correspond)$}$endif$$$^{$author.aff$}$$$sep$ $endfor$
$endif$

$if(affiliation)$
$for(affiliation)$
$$^$affiliation.idx$$${\em $affiliation.aff$} \\
$endfor$
$endif$


$if(email)$
* E-mail: $email$
$endif$

$body$

%$if(bibliography)$
%\nocite{*}
%\bibliographystyle{aipnum-cp}%
%\bibliography{$for(bibliography)$$bibliography$$sep$,$endfor$}%
%$endif$


\end{document}

  |]
