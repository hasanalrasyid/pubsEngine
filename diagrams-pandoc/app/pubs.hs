#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

import qualified Text.Pandoc.Include.Table as IT
import qualified Text.Pandoc.Include.Thesis as IH
import qualified Text.Pandoc.Include.CrossRef as IC
import qualified Text.Pandoc.Include.Markdown as IM
import qualified Text.Pandoc.Include.MultiMarkdown as IMM
import qualified Text.Pandoc.Include.Diagrams as ID
import qualified Text.Pandoc.Include.Delegate as IDel
import qualified Text.Pandoc.Include.FeynMP as IF
import           Text.Pandoc.JSON
import Text.Pandoc.Walk

import Text.Pandoc.CrossRef
import Data.Monoid ((<>))

import Data.Maybe
import System.Environment (getArgs)

import System.Exit

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Template.Poster as P
import qualified Template.Abstract as A
import qualified Template.Thesis as Thesis
import qualified Template.Report as R

main :: IO ()
main = do
  (mdF:format:_) <- getArgs
  mdFile <- TIO.readFile mdF
  doc <- runIO $ readMarkdown def mdFile
  newDoc <- case doc of
             Left err -> error "we have error"
             Right p -> doThemAll p
  result <- runIO $ writeLaTeX (def{writerTemplate = Just $ setTemplate format}) newDoc
  rst <- handleError result
  TIO.putStrLn rst
  where
   setTemplate "poster" = P.templateLatex
   setTemplate "abstract" = A.templateLatex
   setTemplate "Thesis" = Thesis.templateLatex
   setTemplate _ = R.templateLatex

doThemAll (Pandoc mt blks) = do
  blks' <- walkM doBlock blks
  blks'' <- walkM doBlock blks'
  p <- doPandoc (Pandoc mt blks'')
  return p

doPandoc p = doCrossRef =<< ID.addPackagePGF =<< IH.linkTex p

doCrossRef p@(Pandoc meta blocks) = do
  b <- runCrossRefIO meta' (Just $ Format "latex") crossRefBlocks blocks
  return $ Pandoc meta b
    where
      meta' = autoEqnLabels True <> meta

doBlock :: Block -> IO Block
doBlock cb@(CodeBlock (_, classes, namevals) t)
  | "multiTable" `elem` classes = IMM.doInclude cb
  | "feynmp" `elem` classes = IF.doInclude cb
  | "delegate" `elem` classes = IDel.doInclude cb
  | "inputTable" `elem` classes = IT.doInclude cb
  | "include" `elem` classes = IM.doInclude cb
  | "note" `elem` classes = genEnv t  "}" "\\note{"
  | "postbegin" `elem` classes =
    genEnv t "" $
      "\\begin{columns}[t,onlytextwidth] \\begin{column}{0.485\\textwidth}"
  | "postend" `elem` classes =
    genEnv t "" $
      "\\end{column} \\end{columns}"
  | "postseparate" `elem` classes =
    genEnv t "\\begin{column}{0.485\\textwidth}"
             "\\end{column}\\hskip 0.015\\textwidth"
  | "postblockend" `elem` classes =
    genEnv t "" "\\end{block}"
  | "postblockbegin" `elem` classes =
    genEnv t "" $
      "\\begin{block}{\\protect\\textbf{" ++
      (fromMaybe "" $ lookup "caption" namevals) ++
      "}}\n\\justify"
  | "textblock" `elem` classes = do
    let oWidth = fromMaybe "100pt"     $ lookup "w" namevals
    let oLoc   = fromMaybe "10pt,10pt" $ lookup "pos" namevals
    genEnv t "\\end{textblock*}" $ concat["\\begin{textblock*}{", oWidth,"}",oLoc,""]
  where
    genEnv tx en st = do
      tx' <- IM.genPandoc tx
      return $ Div nullAttr $ concat [ [ RawBlock (Format "latex") st ]
                                     , tx'
                                     , [ RawBlock (Format "latex") en ]
                                     ]

doBlock x = return x

data NLine = SingleLine | MultiLine

main1 :: IO ()
main1 = do
  toJSONFilter doThemAll
    {-
  args <- getArgs
  if null args then return ()
               else do
                    putStrLn $ unlines $ "Oda's Lab Thesis filter, run it using:":
                                         "$(stack exec env|grep GHC_PACKAGE_PATH) pandoc -F thesis input.md":[]
                    exitSuccess
    -}

