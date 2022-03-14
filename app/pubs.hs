#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Text.Pandoc.Include.Table as IT
import qualified Text.Pandoc.Include.Thesis as IH
import qualified Text.Pandoc.Include.CrossRef as IC
import qualified Text.Pandoc.Include.Markdown as IM
import qualified Text.Pandoc.Include.MultiMarkdown as IMM
import qualified Text.Pandoc.Include.Diagrams as ID
import qualified Text.Pandoc.Include.Delegate as IDel
import qualified Text.Pandoc.Include.FeynMP as IF
import qualified Text.Pandoc.Mermaid.Filter as Mermaid
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
import qualified Template.Article as Article
import qualified Template.Default as Default

import Text.Pandoc.Include.Common
import qualified Data.Map as M
import System.FilePath.Posix (takeBaseName)
import Data.Aeson (toJSON)
import Control.Monad (forM_)
import System.Process (callCommand)

main :: IO ()
main = do
  (fileName:format:_) <- getArgs
  mdFile <- TIO.readFile $ fileName ++ ".md"
  d <- runIO $  readMarkdown (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting}) Default.templateYaml
  let defaultMeta = case d of
                      Left err -> error "error on meta"
                      Right (Pandoc m _) -> m
  doc <- runIO $ readMarkdown (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting}) mdFile
  newDoc <- case doc of
             Left err -> error "we have error"
             Right p@(Pandoc pM pP) -> do
              putStrLn $ show pM
              doThemAll $ updateMeta defaultMeta p fileName
  template <- setTemplate format
  result <- runIO $ writeLaTeX (def{writerTemplate = Just template, writerTopLevelDivision = TopLevelSection}) newDoc
  rst <- handleError result
  TIO.writeFile ("_build/" ++ fileName ++ ".tex") rst
  let (Pandoc (Meta meta) _) = newDoc
  TIO.putStrLn "======================"
  _ <- case M.lookup "linkDir" meta of
        Nothing -> return $ MetaList []
        Just linkDir -> flip walkM linkDir $ \(Str a) -> do
                          putStrLn $ show a
                          callCommand $ unlines [ "rm -f _build/" ++ a
                                                , unwords [ "ln -s -f",("../" ++ a), "_build/" ++ a ]
                                                ]
                          return $ Str a
  callCommand $ unlines [ "pushd _build"
                        , "pdflatex " ++ fileName ++ ".tex"
                        , "bibtex " ++ fileName
                        , "pdflatex " ++ fileName ++ ".tex"
                        , "pdflatex " ++ fileName ++ ".tex"
                        , "popd" ]
  putStrLn "======================"
  where
   setTemplate "poster" = P.templateLatex
   setTemplate "abstract" = A.templateLatex
   setTemplate "thesis" = Thesis.templateLatex
   setTemplate "report" = R.templateLatex
   setTemplate "article" = Article.templateLatex
   setTemplate _ = R.templateLatex


updateMeta (Meta mt0) (Pandoc (Meta mt) blks) mdFileName =
  let mt' = M.update (\_ -> Just (MetaInlines [ Str mdFileName])) "bibliography" $ M.mapWithKey (updateMeta' mt) mt0
   in Pandoc (Meta mt') blks

-- ("appendix",MetaBlocks [Plain [Str "appendix/app1",SoftBreak,Str "appendix/app2"]])
-- ("appendix",MetaString "")
updateMeta' mt key x = case M.lookup key mt of
                         Nothing -> x
                         Just m@(MetaBlocks a) -> case key of
                                                    "appendix" -> m
                                                    "linkDir" -> m
                                                    _ -> let (MetaBlocks xx) = x
                                                          in MetaBlocks $ xx ++ a
                         Just a -> a

doThemAll (Pandoc mt blks0) = do
  blks1 <- walkM doBlock blks0
  blks  <- walkM doBlock blks1
  p <- doPandoc (Pandoc mt blks)
  return p

doPandoc :: Pandoc -> IO Pandoc
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
  | "mermaid" `elem` classes = Mermaid.doMermaid (Just (Format "latex")) cb
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

