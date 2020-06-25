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

import System.Directory
import System.FilePath
import System.Process
import Text.Pandoc.Include.Common.IO

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
  inDir ("_build" </> "texput" ) $ \f -> do
    _ <- readProcess "xelatex" [] tex
    system_ $ unwords ["mv",f <.> "pdf", "azka.pdf"]

