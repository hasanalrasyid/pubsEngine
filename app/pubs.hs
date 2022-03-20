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
import           Text.Pandoc.JSON
import Text.Pandoc.Walk
import qualified Text.Pandoc.Class as PIO
import qualified Text.Pandoc.Templates as PT

import Data.Monoid ((<>))

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


runIO' :: PandocIO a -> IO a
runIO' f = do
  (res, reports) <- runIOorExplode $ do
    x <- f
    rs <- getLog
    return (x, rs)
  TIO.putStrLn $ T.unlines $ map showLogMessage reports
  return res


main :: IO ()
main = do
  (fileName:format:_) <- getArgs

  callCommand "mkdir -p _build/{auto,temp/lib/py,temp/lib/sh}"
  mdIncludedPandoc@(Pandoc resMeta _) <- runIO' $ do
    mdFile <- fmap TE.decodeUtf8 $ PIO.readFileStrict $ fileName <> ".md"
    readMarkdown (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting}) mdFile
      >>= walkM includeMarkdown

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

  resPandoc@(Pandoc t2 p2 ) <- doThemAll mdIncludedPandoc
  (tFileName, tFile) <- Article.templateLatex
  resLatex <- runIO' $ do
    template <- runWithPartials $ PT.compileTemplate tFileName $ T.pack tFile
    case template of
      Left e -> error e
      Right t -> writeLaTeX (def{writerTemplate = Just t, writerTopLevelDivision = TopLevelSection}) resPandoc
  TIO.writeFile ("_build/" <> fileName <> ".tex") resLatex
  putStrLn "==============================="
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
    setTemplate "poster" = P.templateLatex
    setTemplate "abstract" = A.templateLatex
    setTemplate "thesis" = Thesis.templateLatex
    setTemplate "report" = R.templateLatex
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
                                                          in MetaBlocks $ xx <> a
                         Just a -> a

doThemAll (Pandoc mt blks0) = do
  let (imageDirs :: [String]) = case lookupMeta "imageDir" mt of
                    Just (MetaList a) -> concat $ flip map a $ \(MetaInlines i) -> flip map i $ \(Str s) -> T.unpack s
                    _ -> []
  putStrLn "imageDirs =============================="
  putStrLn $ show imageDirs
  blks <- walkM includeScript blks0 >>= walkM doBlockIO >>= walkM doBlockIO >>= walkM (upgradeImageIO imageDirs)
  p <- doPandoc (Pandoc mt blks)
  return p

walkM_ a b = () <$ walkM a b

doPandoc p = Diagrams.addPackagePGF =<< linkTex p

mdOption = (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting})

upgradeImageIO :: [String] -> Block -> IO Block
-----------------------------------------zShell----------------------------------------
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


includeScript :: Block -> IO Block
-----------------------------------------zShell----------------------------------------
includeScript cb@(CodeBlock (label, ["script","sh","lib"], opts) script) = do
  let fileName = T.unpack $ fromMaybe "shLibDefault" $ lookup "file" opts
  TIO.writeFile ("_build/temp/lib/sh" </> fileName <.> "sh") script
  r <- readProcess "zsh" [] $ T.unpack $ T.unlines [script,"echo $description"]
  res <- runIO $ readMarkdown mdOption $ T.pack r
  return $ case res of
    Left e -> Null
    Right (Pandoc _ b) -> Div nullAttr b

includeScript cb@(CodeBlock (label, classes@["script","sh",outType], opts) script) = do
  files <- listDirectory "_build/temp/lib/sh"
  let header = unlines $ map (\f -> ". _build/temp/lib/sh" </> f) files
      s = unlines [header,T.unpack script]
  putStrLn s
  res <- readProcess "zsh" [] s
  case outType of
    "md" -> do
            r <- runIO $ readMarkdown mdOption $ T.pack res
            return $ Div nullAttr $ case r of
              Left e -> [Para [Str $ T.pack $ "ERROR: script.sh.md: cannot parse the markdown output: " <> show e]]
              Right (Pandoc _ b) -> b
    "img" -> do
      let fileName = fromMaybe "zshImg" $ lookup "file" opts
          caption = fromMaybe "" $ lookup "caption" opts
          width  = fromMaybe "800" $ lookup "width" opts
          height = fromMaybe "600" $ lookup "height" opts
      return $ Div nullAttr [Para [Image (label,[],opts) [Str caption] (fileName, label) ]]
    _ -> do
      return $ Div nullAttr [Para [Str $ T.pack $ "ERROR: unacceptable script class headers, we got: " <> show classes ]]

-----------------------------------------Python----------------------------------------
includeScript cb@(CodeBlock (label, ["script","py","lib"], opts) script) = do
  let fileName = T.unpack $ fromMaybe "pyLibDefault" $ lookup "file" opts
  TIO.writeFile ("_build/temp/lib/py" </> fileName <.> "py") script
  r <- readProcess "python3" [] $ T.unpack $ T.unlines [script,"print(description)"]
  res <- runIO $ readMarkdown mdOption $ T.pack r
  return $ case res of
    Left e -> Null
    Right (Pandoc _ b) -> Div nullAttr b
includeScript cb@(CodeBlock (label, classes@["script","py",outType], opts) script) = do
  TIO.writeFile ("_build/temp/script.py") script
  files <- fmap (delete "__pycache__")$ listDirectory "_build/temp/lib/py"
  callCommand "chmod +x _build/temp/script.py"
  let header = unlines [ "import sys, os"
                       , unlines $ map (\f -> unwords ["from _build.temp.lib.py." <> f,"import *"] ) $ map takeBaseName files
                       ]
      s = unlines [header,T.unpack script]
  putStrLn s
  res <- readProcess "python3" [] s
  case outType of
    "md" -> do
            r <- runIO $ readMarkdown mdOption $ T.pack res
            case r of
              Left e -> error $ show e
              Right (Pandoc _ b) -> return $ Div nullAttr b
    "img" -> do
      let fileName = case lookup "file" opts of
                                         Nothing -> "script"
                                         Just a -> a
          caption = fromMaybe "No caption" $ lookup "caption" opts
          width  = fromMaybe "800" $ lookup "width" opts
          height = fromMaybe "600" $ lookup "height" opts
      return $ Div nullAttr [Para [Image (label,[],opts) [Str caption] (fileName, label) ]]
    _ -> do
      return $ Div nullAttr [Para [Str $ "ERROR: unacceptable script class headers, we got: " <> (T.pack $ show classes) ]]

includeScript cb@(CodeBlock (a, ("script":_), opts) t) =
  includeScript $ CodeBlock (a,["script","py","md"], opts) t
includeScript cb = return cb



includeMarkdown :: Block -> PandocIO Block
includeMarkdown cb@(CodeBlock (label, ["include"], _) t) = do
  let fileList = lines $ T.unpack t
  (inMd :: [[Block]]) <- flip mapM fileList $ \f -> do
    fMd <- TE.decodeUtf8 <$> PIO.readFileStrict f
    r <- readMarkdown def fMd
    (Pandoc _ s) <- walkM includeMarkdown r
    return s
  return $ Div (label,[],[]) $ concat inMd
includeMarkdown x = return x

doBlockIO cb@(CodeBlock (_, classes, namevals) t)
  | "multiTable" `elem` classes = MultiMarkdown.doInclude cb
  | "feynmp" `elem` classes = FeynMP.doInclude cb
  | "mermaid" `elem` classes = Mermaid.doInclude cb
  | "delegate" `elem` classes = Delegate.doInclude cb
  | "inputTable" `elem` classes = InTable.doInclude cb
  | "include" `elem` classes = Markdown.doInclude cb
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
