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
import System.Process (callCommand)

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Text.Pandoc.App

main :: IO ()
main = do
  (fileName:format:_) <- getArgs

  let runIO' :: PandocIO a -> IO a
      runIO' f = do
        (res, reports) <- runIOorExplode $ do
--                           setTrace (optTrace opts)
--                           setVerbosity verbosity
                             x <- f
                             rs <- getLog
                             return (x, rs)
--      case optLogFile opts of
--           Nothing      -> return ()
--           Just logfile -> putStrLn $ logfile (encodeLogMessages reports)
        let isWarning msg = messageVerbosity msg == WARNING
--      when (optFailIfWarnings opts && any isWarning reports) $
--          E.throwIO PandocFailOnWarningError
        return res

  putStrLn "1==========================================="
  resPandoc@(Pandoc resMeta _) <- runIO' $ do
    mdFile <- PIO.readFileStrict $ fileName <> ".md"
    res <- readMarkdown (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting}) $ TE.decodeUtf8 mdFile
    walkM includeMarkdown res
    -- return res
  putStrLn $ show resPandoc
  putStrLn "2=============================="
  putStrLn $ show resMeta
  r1 <- walkM doBlockIO resPandoc
  putStrLn "3=============================="
  case lookupMeta "linkDir" resMeta of
    Just (MetaList linkDirs) -> do -- putStrLn $ show linkDirs
      flip walkM_ linkDirs $ \l@(Str link) -> do
        TIO.putStrLn $ "Creating symbolink link to: " <> link
        callCommand $ T.unpack $ T.unlines [ "rm -f _build/" <> link
                                           , T.unwords [ "ln -s -f",("../" <> link), "_build/" <> link ]
                                           ]
        return l
    _ -> putStrLn "no linkDir available"
  putStrLn "4=============================="
  r2@(Pandoc t2 p2 ) <- doThemAll r1
  putStrLn $ show t2
  putStrLn "n=============================="
  (tFileName, tFile) <- Article.templateLatex -- format
--  let template = (templateTex <$) <$> writerTemplate def

  resLatex <- runIO' $ do
    template <- runWithPartials $ PT.compileTemplate tFileName $ T.pack tFile
    case template of
      Left e -> error e
      Right t -> writeLaTeX (def{writerTemplate = Just t, writerTopLevelDivision = TopLevelSection}) r2
  TIO.writeFile ("_build/" <> fileName <> ".tex") resLatex
  putStrLn $ show resLatex
  putStrLn "==============================="




--(defaultMeta,p) <- runIO' $ do
--    (Pandoc defaultMeta _) <- readMarkdown (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting}) Default.templateYaml
--    (r :: Pandoc) <- readMarkdown (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting}) mdFile
--    return (defaultMeta,r)
--  inputDoc  <- doThemAll $ updateMeta defaultMeta p $ T.pack fileName
--  putStrLn $ show inputDoc
--  putStrLn $ show defaultMeta

  {-
  t0 <- T.pack <$> setTemplate format
  putStrLn $ show $ (t0 <$) <$> writerTemplate def
  result <- runIO $ do
    let template = (t0 <$) <$> writerTemplate def
    writeLaTeX (def{writerTemplate = template, writerTopLevelDivision = TopLevelSection}) newDoc
  rst <- handleError result
  TIO.writeFile ("_build/" <> fileName <> ".tex") rst
  let (Pandoc (Meta meta) _) = newDoc
  0TIO.putStrLn "======================"
  -}
-- _ <- case M.lookup "linkDir" meta of
--       Nothing -> return $ MetaList []
--       Just linkDir -> flip walkM linkDir $ \(Str a) -> do
--                         putStrLn $ show a
--                         callCommand $ T.unpack $ T.unlines [ "rm -f _build/" <> a
--                                               , T.unwords [ "ln -s -f",("../" <> a), "_build/" <> a ]
--                                               ]
--                         return $ Str a
-- callCommand $ unlines [ "pushd _build"
--                       , "pdflatex " <> fileName <> ".tex"
--                       , "bibtex " <> fileName
--                       , "pdflatex " <> fileName <> ".tex"
--                       , "pdflatex " <> fileName <> ".tex"
--                       , "popd" ]
-- putStrLn "======================"
  where
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
  blks  <- walkM doBlockIO blks0
  p <- doPandoc (Pandoc mt blks)
  return p

walkM_ a b = () <$ walkM a b

doPandoc p = Diagrams.addPackagePGF =<< linkTex p

includeMarkdown :: Block -> PandocIO Block
includeMarkdown cb@(CodeBlock (_, ["include"], namevals) t) = do
  let fileList = lines $ T.unpack t
  (inMd :: [[Block]]) <- flip mapM fileList $ \f -> do
    fMd <- TE.decodeUtf8 <$> PIO.readFileStrict f
    r <- readMarkdown def fMd
    (Pandoc _ s) <- walkM includeMarkdown r
    return s
  return $ Div nullAttr $ concat inMd
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
