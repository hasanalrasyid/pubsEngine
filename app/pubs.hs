#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Text.Pandoc.Include.Table as InTable
import Text.Pandoc.Include.Thesis (linkTex)
import qualified Text.Pandoc.Include.Markdown as Markdown
import qualified Text.Pandoc.Include.MultiMarkdown as MultiMarkdown
import qualified Text.Pandoc.Include.Diagrams as Diagrams
import qualified Text.Pandoc.Include.Delegate as Delegate
import qualified Text.Pandoc.Include.FeynMP as FeynMP
import qualified Text.Pandoc.Include.Mermaid as Mermaid
import qualified Text.Pandoc.Include.GoJS as GoJS
import           Text.Pandoc.Include.Script
import           Text.Pandoc.Include.Common.IO
import           Text.Pandoc.JSON
import           Text.Pandoc.Definition
import Text.Pandoc.Walk
import qualified Text.Pandoc.Class as PIO
import qualified Text.Pandoc.Templates as PT

import Control.Monad

import Data.Monoid

import Data.Maybe
import System.Environment (getArgs)

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
import Text.Pandoc.Include.Thesis (processPreDoc, processPostDoc)
import qualified Data.Map as M
import System.Process (callCommand, readProcess)

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Text.Pandoc.App
import System.FilePath -- (replaceDirectory)
import System.Directory -- (listDirectory)
import Data.List -- (delete)
import Text.Pandoc.Citeproc
import Text.Pandoc.Shared


main :: IO ()
main = do
  (fileName:format:_) <- getArgs

  callCommand "mkdir -p _build/{auto,temp/lib/py,temp/lib/sh,temp/lib/gnuplot}"
  (Pandoc (Meta resMeta0) resP) <- runIO' $ do
    mdFile <- fmap TE.decodeUtf8 $ PIO.readFileStrict $ fileName <> ".md"
    readMarkdown mdOption mdFile
      >>= walkM processPreDoc
      >>= walkM Markdown.includeMarkdown
  let resMeta = Meta $ M.alter (\_ -> Just (MetaBool True)) "link-citations"
        $ M.alter (\_ -> Just (MetaBool True)) "link-bibliography"
        $ M.alter (\_ -> Just (MetaInlines [Str $ T.pack $ fileName <> ".bib" ])) "bibliography" resMeta0
  case lookupMeta "imageDir" resMeta of
    Just (MetaList linkDirs) -> do
      flip walkM_ linkDirs $ \l@(Str link) -> do
        TIO.putStrLn $ "Creating symbolink link to: " <> link
        callCommand $ T.unpack $ T.unlines
          [ "rm -f _build/" <> link
          , T.unwords [ "ln -s -f",("../" <> link), "_build/" <> link ]
          ]
        return l
    _ -> putStrLn "no linkDir available"

  case lookupMeta "bibzotero" resMeta of
    Just (MetaInlines [Str bibzotero]) -> do
      statZotero <- readProcess "pgrep" ["zotero"] []
      case statZotero of
        [] -> error $ unlines [ "ERROR: bibzotero: markdown option for zotero connection is set as " <> T.unpack bibzotero
                              , "                  but the standalone Zotero with better-bibtex addons is not running."
                              ]
        _ -> callCommand $ T.unpack $ "curl http://127.0.0.1:23119/better-bibtex/export/collection\\?/1/"<>bibzotero<>".bibtex > "<>T.pack fileName<>".bib"
    b -> do
      putStrLn $ "WARNING: bibzotero: no connection to zotero bibliography is provided in markdown option of " <> show b
      putStrLn $ "                    Fallback to using " <> fileName <> ".bib in the current directory"
  callCommand $ unwords ["ln -sf", "../" <>fileName <> ".bib", "_build/" <> fileName <> ".bib" ]

  r2 <- doThemAll $ Pandoc resMeta resP
  (tFileName, tFile, topLevel) <- setTemplate format
  let (Pandoc t3 p3 ) = processPostDoc r2
  let (varMeta :: Meta) = Meta $ M.fromList $ catMaybes $ map getVars p3
  let p4 = walk cleanVariable $ walk (fillVariable varMeta) p3
  resLatex <- runIO' $ do

    citedPandoc <- processCitations $ Pandoc t3 p4
    template <- runWithPartials $ PT.compileTemplate tFileName $ T.pack tFile
    case template of
      Left e -> error e
      Right t -> writeLaTeX (def{writerTemplate = Just t, writerTopLevelDivision = topLevel}) citedPandoc
  TIO.writeFile ("_build/" <> fileName <> ".tex") resLatex
  putStrLn "==============================="
  putStrLn $ show t3
  compileLatex fileName
  where
    compileLatex fileName = do
      callCommand $ unlines [ "pushd _build"
                            , "xelatex " <> fileName <> ".tex"
                            , "bibtex  " <> fileName
                            , "xelatex " <> fileName <> ".tex"
                            , "xelatex " <> fileName <> ".tex"
                            , "popd" ]
      putStrLn "======================"
    --setTemplate "poster" = P.templateLatex
    --setTemplate "abstract" = A.templateLatex
    --setTemplate "report" = R.templateLatex
    setTemplate "article" =  Article.templateLatex
    setTemplate "thesis" =  Thesis.templateLatex
    setTemplate _ = Article.templateLatex


doThemAll (Pandoc mt blks0) = do
  let (imageDirs :: [String]) = case lookupMeta "imageDir" mt of
                    Just (MetaList a) -> concat $ flip map a $ \(MetaInlines i) -> flip map i $ \(Str s) -> T.unpack s
                    _ -> []
  putStrLn "imageDirs =============================="
  putStrLn $ show imageDirs
  blks1 <- flip walkM blks0
            $ includeScript
            >=> doBlockIO
            >=> doBlockIO
            >=> GoJS.includeGoJS
            >=> upgradeImageIO imageDirs
  blks <- walkM doBlockIO blks1
  p <- doPandoc (Pandoc mt blks)
  return p

cleanVariable :: Block -> Block
cleanVariable (Div (_,["var",varName],_) _) = Null
cleanVariable p = p

fillVariable :: Meta -> Inline -> Inline
fillVariable varMeta p@(Cite [c] _)
  | T.isPrefixOf "var:" $ citationId c =
      let filler = join $ fmap (flip lookupMeta varMeta) $ T.stripPrefix "var:" $ citationId c
       in case filler of
            Just (MetaBlocks s) -> Span nullAttr $ blocksToInlines s
            _ -> Str ""
  | otherwise = p
fillVariable _ p = p

getVars (Div (_,["var",varName],_) bs) = Just (varName , MetaBlocks bs)
getVars c = Nothing

walkM_ a b = () <$ walkM a b

doPandoc p = Diagrams.addPackagePGF =<< linkTex p

upgradeImageIO :: [String] -> Block -> IO Block
-----------------------------------------UpgradeImage----------------------------------------
upgradeImageIO dirList cb@(Para [Image (l1,[],opts) caption (fileName, l2)]) = do
  latex <- runIO $ writeLaTeX def $ Pandoc nullMeta [cb]
  case latex of
    Left _ -> return cb
    Right a -> do
      TIO.writeFile "_build/temp/upgradeImageIO.tmp" a
      let width = fromMaybe "1.0" $ lookup "size" opts
      r <- readProcess "zsh" [] $ "sed -e 's/\\(includegraphics\\){\\|\\[\\([^]]*\\)\\]{/\\1[keepaspectratio=true,width="<> T.unpack width <>"\\\\linewidth]{/g' _build/temp/upgradeImageIO.tmp"
      callCommand "rm -f _build/temp/upgradeImageIO.tmp"
      return $ RawBlock (Format "latex") $ T.pack r
upgradeImageIO _ c = return c

doBlockIO cb@(CodeBlock (_, classes, namevals) t)
  | "multiTable" `elem` classes = MultiMarkdown.doInclude cb
  | "feynmp" `elem` classes = FeynMP.doInclude cb
  | "mermaid" `elem` classes = Mermaid.doInclude cb
  | "delegate" `elem` classes = Delegate.doInclude cb
  | "inputTable" `elem` classes = InTable.doInclude cb
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
      "\\begin{block}{\\protect\\textbf{" <>
      (fromMaybe "" $ lookup "caption" namevals) <>
      "}}\n\\justify"
  | "textblock" `elem` classes = do
    let oWidth = fromMaybe "100pt"     $ lookup "w" namevals
    let oLoc   = fromMaybe "10pt,10pt" $ lookup "pos" namevals
    genEnv t "\\end{textblock*}" $ T.concat["\\begin{textblock*}{", oWidth,"}",oLoc,""]
  where
    genEnv :: T.Text -> T.Text -> T.Text -> IO Block
    genEnv tx en st = do
      tx' <- Markdown.genPandoc tx
      return $ Div nullAttr $ concat [ [ RawBlock (Format "latex") st ]
                                     , tx'
                                     , [ RawBlock (Format "latex") en ]
                                     ]

doBlockIO x = return x

data NLine = SingleLine | MultiLine

    {-
main1 :: IO ()
main1 = do
  toJSONFilter doThemAll
  args <- getArgs
  if null args then return ()
               else do
                    putStrLn $ unlines $ "Oda's Lab Thesis filter, run it using:":
                                         "$(stack exec env|grep GHC_PACKAGE_PATH) pandoc -F thesis input.md":[]
                    exitSuccess
    -}
