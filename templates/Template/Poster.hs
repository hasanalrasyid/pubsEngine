{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Template.Poster where

import Text.RawString.QQ

templateLatex = [r|

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local Variables:
%%% mode: latex
%%% TeX-PDF-mode: t
% Options for packages loaded elsewhere
%--%\PassOptionsToPackage{unicode$for(hyperrefoptions)$,$hyperrefoptions$$endfor$}{hyperref}
%--%\PassOptionsToPackage{hyphens}{url}
%--%$if(colorlinks)$
%--%\PassOptionsToPackage{dvipsnames,svgnames*,x11names*}{xcolor}
%--%$endif$
%--%$if(dir)$
%--%$if(latex-dir-rtl)$
%--%\PassOptionsToPackage{RTLdocument}{bidi}
%--%$endif$
%--%$endif$
%
\documentclass[final]{beamer}
\usetheme{I6dv}
\usefonttheme{professionalfonts} % using non standard fonts for beamer
%\usefonttheme{serif}
%\usefonttheme[serif]{structurebold}% default family is serif
%\usepackage{fontspec}
%\setmainfont{"Apercu Bold"}
% further beamerposter themes are available at
% http://www-i6.informatik.rwth-aachen.de/~dreuw/latexbeamerposter.php
 \usepackage{type1cm}
 \usepackage{calc}
 \usepackage{times}
 \usepackage{amsmath,amsthm, amssymb, latexsym}

\usepackage[numbers]{natbib}
\usepackage{rotating}
\usepackage{graphicx}

%$if(csl-refs)$
%\newlength{\cslhangindent}
%\setlength{\cslhangindent}{1.5em}
%\newenvironment{cslreferences}%
%  {$if(csl-hanging-indent)$\setlength{\parindent}{0pt}%
%  \everypar{\setlength{\hangindent}{\cslhangindent}}\ignorespaces$endif$}%
%  {\par}
%$endif$
% you can chose your theme here:
\usepackage{tikz}
\usepackage{tikz-bpmn}
\usetikzlibrary{positioning}
\usepackage[utf8]{inputenc} %or: \usepackage[latin1]{inputenc}
 \boldmath
 \usepackage[english]{babel}
 \usepackage[orientation=portrait,size=a0,scale=0.9]{beamerposter}
\usepackage{gensymb}
\setbeamersize{text margin left=18mm}
\setbeamersize{text margin right=18mm}
$for(header-includes)$
$header-includes$
$endfor$
$for(add-header-includes)$
$add-header-includes$
$endfor$

% Title portion
$if(title)$
\title{$title$$if(subtitle)$\\\LARGE{$subtitle$}$endif$}
$endif$


$if(author)$
\author{ $for(author)$$if(author.newline)$\\ $endif$$if(author.correspond)$\underline{$endif$$author.name$$if(author.correspond)$}$endif$$$^{$author.aff$}$$$sep$ $endfor$}
$endif$
\institute{
$if(affiliation)$
$for(affiliation)$
$$^{$affiliation.idx$}$$$affiliation.aff$ \\
$endfor$
$endif$$if(email)$\textit{ E-mail address: $email$}$endif$}

\setbeamerfont{itemize/enumerate subbody}{size=\normalsize}
\setbeamerfont{itemize/enumerate subsubbody}{size=\normalsize}

%\setbeamertemplate{footline}{
%  \begin{beamercolorbox}[wd=\paperwidth]{upper separation line foot}
%    \rule{0pt}{2pt}
%  \end{beamercolorbox}
%
% \begin{beamercolorbox}{footline}%
%   \vskip 0.7em
%   \begin{columns}[t]
%     \begin{column}{0.025\linewidth}
%     \end{column}
%     \begin{column}{0.19\linewidth}
%       \center
%       $if(logo)$
%        $for(logo)$
%       \includegraphics[height=6em]{$logo$}
%       $endfor$
%       $endif$
%     \end{column}
%     \begin{column}{0.025\linewidth}
%     \end{column}
%
%   \end{columns}
%   \vskip 1.2em
% \end{beamercolorbox}
%}

\usepackage{ragged2e}

% Document starts
\begin{document}

$for(include-before)$
$include-before$
$endfor$

\justify

\begin{frame}[t]{}
  \vskip -0.5em

  $body$

\end{frame}
\end{document}
  |]
