#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Random
import Data.List.Split
import Data.List

  {-
     For optimization, we should expect several cell parameters needs to be optimized.
     in this case, we may have 1 to 6 parameter of a,b,c,alpha,beta,gamma
     with minimum variation on alpha,beta,gamma.
     however, in generalized term, then we may construct optimization function:
     (x1 - a)(x1 - b)(x2 - c)(x2 - d)(x3 - e)(x3 - f) ... - konst = 0
     -}

main :: IO ()
main = do
  g <- newStdGen
  r <- newStdGen
  let pm =(randomRs (True,False) r)
  let (xx :: [(Bool,[Int])]) =  take 720 $ filter (\(p,(a:b:_)) -> if p then a + b /= 0 else (a-b) >= 0 ) $ zip pm $ chunksOf 2 (randomRs (0,10) g)
  --putStrLn $ unlines $ take 20 $ map (\(a:b:_) -> unwords  $ ["$",show a , " - ", show b, " = $"]) xx

  let tex = unlines [ "\\documentclass{article}"
                    , "\\usepackage{amsmath}"
                    , "\\usepackage{feynmp-auto}"
                    , "\\usepackage[a4paper,bindingoffset=0cm,left=1cm,right=1cm,top=1cm,bottom=1cm,footskip=.25in]{geometry}"
                    , "\\usepackage{multicol}"
                    , "\\renewcommand{\\baselinestretch}{1.5}"
                    , "\\begin{document}"
                    , "\\pagestyle{empty}"
                    , "\\begin{multicols}{4}"
                    , "\\noindent"
                    , "\\Huge"
                    , unlines $ map (\(p,(a:b:_)) -> unwords ["$",show a , if p then "+" else "-", show b, " = $ \\\\"]) xx
                    , "\\end{multicols}"
                    , "\\end{document}"
                    ]
  putStrLn tex
