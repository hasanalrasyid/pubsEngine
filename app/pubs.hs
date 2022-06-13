#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.IO.Class (liftIO)
import qualified Text.Pandoc.Include.Table as InTable
import Text.Pandoc.Include.Thesis (linkTex)
import qualified Text.Pandoc.Include.Markdown as Markdown
import qualified Text.Pandoc.Include.MultiMarkdown as MultiMarkdown
import qualified Text.Pandoc.Include.Diagrams as Diagrams
import qualified Text.Pandoc.Include.Delegate as Delegate
import qualified Text.Pandoc.Include.FeynMP as FeynMP
import qualified Text.Pandoc.Include.Chem as Chem
import qualified Text.Pandoc.Include.Mermaid as Mermaid
import qualified Text.Pandoc.Include.GoJS as GoJS
import qualified Text.Pandoc.Include.PlantUML as PlantUML
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

import qualified Text.Pandoc.Include.Template as Template
import           Text.Pandoc.Include.Template.Book

import Text.Pandoc.Include.Common
import Text.Pandoc.Include.Utils
import qualified Text.Pandoc.Include.Nusantara as NU
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
import Text.Pandoc.Readers.Markdown

import Text.Pandoc.Process
import qualified Data.ByteString.Lazy as BL
import qualified Text.Pandoc.UTF8 as UTF8

import System.Exit(ExitCode(..))

import Text.Pandoc.CrossRef

import Text.Pandoc.Builder (str,header,text,displayMath)
import Text.Regex

cpOS = "rsync -avr"

main :: IO ()
main = do
  (fileName:nameTemplate:_) <- getArgs

  mapM callCommand $ map ("mkdir -p _build/"<>) $ ["auto","temp/lib/py","temp/lib/sh","temp/lib/gnuplot"]
  (template,Pandoc (Meta resMeta0) resP) <- runIO' $ do
    resT <- genTemplate nameTemplate fileName
    liftIO $ putStrLn $ show resT
    mdFile <- fmap TE.decodeUtf8 $ PIO.readFileStrict $ fileName <> ".md"
    (Pandoc (Meta m1) p1) <- readMarkdown mdOption mdFile
      >>= walkM Markdown.includeMarkdown
    (Meta mYaml) <- do
      yamlExist <- fileExists $ fileName <> ".yaml"
      if yamlExist then do
                    yamlFile <- PIO.readFileStrict $ fileName <> ".yaml"
                    yamlToMeta mdOption Nothing yamlFile
                   else pure defaultMeta
    -- update mYaml, use the values from yaml in .md
    let mRes = Meta $ M.foldlWithKey (\a k v -> M.alter (\_ -> Just v) k a) mYaml m1
    return $ (resT, Pandoc mRes p1)
  case lookupMeta "link-directory" $ Meta resMeta0 of
    Just linkDirs -> flip walkM_ linkDirs $ \l@(Str link) -> do
                      TIO.putStrLn $ "Creating symbolink link to: " <> link
                      callCommand $ T.unpack $ T.unlines
                        [ "rm -fr _build/" <> link
                        , T.unwords [ T.pack cpOS, link, "_build/" ]
                        ]
                      return l
    _ -> putStrLn "no linkDir available"

  bibliographyFile <- case lookupMeta "zotero-collection" $ Meta resMeta0 of
    Just bib -> do
      flip query bib $ \(Str zoteroCollection0) -> do
        let zoteroCollection = T.unpack zoteroCollection0
        (ex,statZotero) <- pipeProcess Nothing "pgrep" ["zotero"] ""
        case ex of
          ExitSuccess -> do
            mapM_ callCommand
              [ "curl 'http://127.0.0.1:23119/better-bibtex/export/collection-archive?/1/"<>zoteroCollection<>".biblatex&exportNotes=true'  --output _build/temp/"<>zoteroCollection<>".zip.base64"
              , "base64 -d _build/temp/"<>zoteroCollection<>".zip.base64 > _build/temp/"<>fileName<>".zip"
              , "unzip -o _build/temp/"<>fileName<>".zip -d _build/"
              , "rm -rf _build/temp/bibliography"
              , "mv _build/"<>zoteroCollection<>" _build/temp/bibliography"
              , "pandoc -t CSLjson  _build/temp/bibliography/"<>zoteroCollection<>".bib |sed -e '/\"note\":/d' | pandoc -t bibtex -f CSLjson > _build/" <> fileName <> ".bib"
              , unwords ["cp -f _build/" <> fileName <> ".bib", "." ]
              ]
          _ -> do
            putStrLn $ unlines [ "ERROR: zotero-collection: markdown option for zotero connection is set as " <> zoteroCollection
                               , "                          but the standalone Zotero with better-bibtex addons is not running."
                               , "                          Fallback into simple mode"
                               ]
        return zoteroCollection0
    b -> do
      putStrLn $ "WARNING: zotero-collection: no connection to zotero bibliography is provided in markdown option of " <> show b
      putStrLn $ "                            Fallback to using " <> fileName <> ".bib in the current directory"
      callCommand $ unwords [ cpOS, fileName <> ".bib", "_build/" ]
      return $ T.pack fileName
  let resMeta = Meta
        $ M.alter (\_ -> Just (MetaBool True)) "link-citations"
        $ M.alter (\_ -> Just (MetaBool True)) "link-bibliography"
        $ M.alter (\_ -> Just (MetaInlines [Str "_build/reference.csl" ])) "csl"
        $ M.alter (\_ -> Just (MetaBool True)) "suppress-bibliography"
        $ M.alter (\_ -> Just (MetaInlines [Str $ T.pack $ fileName <> ".bib"])) "bibliography" resMeta0

  (Pandoc (Meta t3) p3 ) <- doThemAll nameTemplate $ Pandoc resMeta resP
  p4 <- walkM (NU.processPegonInline nameTemplate) p3
  templateParams  <- Template.setTemplate nameTemplate fileName

  let varNotationContent = M.singleton "content" $ MetaList $ query getNotation p4
      alter' k m f = M.alter f k m
      tNotation = alter' "notation" t3 $
        \case
          Nothing -> Just $ MetaMap varNotationContent
          Just (MetaMap n) -> Just $ MetaMap $ M.union n varNotationContent
  let (varMeta) = M.fromList $ catMaybes $ map getVars p4
  let m6 = Meta $ flip M.union tNotation varMeta

  let p6 = doBook nameTemplate $ Pandoc m6 $ walk processAcknowledgements $ walk cleanVariable $ walk (fillVariableI varMeta) $ walk (fillVariableB varMeta) p4
  let (imageDirs :: [String]) = case lookupMeta "imageDir" m6 of
                    Just (MetaList a) -> concat $ flip map a $ \(MetaInlines i) -> flip map i $ \(Str s) -> T.unpack s
                    _ -> []
  citedPandoc <- runIO' $ do
    Pandoc mc1 t <- processShowCitations bibliographyFile fileName p6
    t1 <- liftIO $ walkM upgradeImageInline t
    fmap (walk processBlockNote . processSupplementary) $ processCitations $ Pandoc mc1 t1
  finishDoc template nameTemplate templateParams fileName citedPandoc


processBlockNote a@(Note [Div _ ((BlockQuote b):c)]) = Note $ b <> c
processBlockNote a@(Note ((BlockQuote b):c)) = Note $ b <> c
processBlockNote b = b

isSupplement (Header 1 (_,["supplement"],_) _) = True
isSupplement (Div _ ((Header 1 (_,["supplement"],_) _):_)) = True
isSupplement b = False
processSupplementary (Pandoc m b) =
  let b' = case findIndex isSupplement b of
            Nothing -> b
            Just i ->
              let (h,t) = splitAt i b
               in h ++ (RawBlock (Format "latex") "\\backmatter"):t
      bmhead = walk genSupplementary b'
   in Pandoc m bmhead


genSupplementary (Header 1 (_,["supplement"],_) l) = do
  let bm = RawInline (Format "latex") "\\bmhead{"
   in Plain $ concat [[bm], l, [RawInline (Format "latex") "}"]]
genSupplementary h = h

processShowCitations bibliographyFile fileName p@(Pandoc m _) = do
  bibFile <- fmap TE.decodeUtf8 $ PIO.readFileStrict $ fileName <> ".bib"
  (Pandoc bib _) <- readBibTeX def bibFile
  let notes = lookupMeta "references" bib
  walkM (genNotes notes) p
    where
      eqCitationId c (MetaMap n) =
        (Just $ MetaString $ citationId c) == M.lookup "id" n
      eqCitationId _ _ = False
      getNote notes a@(Cite [c] _) =
        case find (eqCitationId c) notes of
          Just (MetaMap n) ->
            let note =  case M.lookup "note" n of
                          Just (MetaInlines l) -> l
                          _ -> []
             in Just ([Str $ "@" <> citationId c],[[Para note]])
          _ -> Nothing
      formatNote bibliographyFile (Image a@(_,_,v) i (_,u)) =
        let target = fromMaybe "." $ lookup "attachment-key" v
        -- add caption here
         in Image ("fig:"<>target,[],[]) i ("temp/bibliography/" <> target <> "/image.png",u)
      formatNote _ LineBreak = Space
      formatNote _ i = i
      formatNoteBlock (Header _ _ l) = Para [Strong l]
      formatNoteBlock (Para ((Image a@(target,_,_) _ l):captionCandidate)) =
        Para [Image a ((Strong [Str "CiteNote: "]):captionCandidate) l]
      formatNoteBlock i = i
      getNotes _ _ [] [] = pure Null
      getNotes _ _ res [] = pure $ DefinitionList $ catMaybes res
      getNotes bibliographyFile notes res ((Cite [c] _):ns) = do
        r <- case find (eqCitationId c) notes of
              Just (MetaMap n) -> do
                let cId = citationId c
                note <- do
                  -- jq -r '.items|map ({citationKey: .citationKey,notes:(.notes|map(.note))})' pubsEngine/pubsEngine.json
                  (ex, noteHTML0) <- liftIO $ pipeProcess Nothing "jq"
                    [ "-r", "--arg", "citationId", T.unpack cId
                    , ".items|.[]|select(.citationKey == $citationId)|.notes|map(.note)|.[]"
                    , "_build/temp/bibliography/" <> T.unpack bibliographyFile <> ".json"
                    ] ""
                  case ex of
                    ExitSuccess -> pure ()
                    err -> error $ "getNotes: jq: bibliography: " <> show err
                  Pandoc _ noteHTML <- do
                    h <- readHtml mdOption $ T.replace "<img" "<img src='dummy.jpg'" $ UTF8.toText $ BL.toStrict noteHTML0
                    pure $ walk formatNoteBlock $ walk (formatNote bibliographyFile) h
                  pure noteHTML
                pure $ Just ([Str $ "@" <> cId],[note])
              _ -> pure Nothing
        getNotes bibliographyFile notes (r:res) ns
      genNotes (Just (MetaList notes)) b = do
        notes <- getNotes bibliographyFile notes [] $ query checkNote b
        pure $ Div nullAttr $ [ b, notes ]
      genNotes _ b = pure b
      checkShow a [Str ".show"] = [a]
      checkShow _ _ = []
      checkNote a@(Cite [c] _) =
        let checkShow [] = []
            checkShow s
              | last s == Str ".show" = [a]
              | otherwise = []
         in checkShow $ citationSuffix c
      checkNote _ = []

processCrossRef p@(Pandoc meta _)= runCrossRefIO meta (Just "latex") action p
  where
    action (Pandoc _ bs) = do
      --meta' <- crossRefMeta # do not modify meta for crossRef due to incompatible \usepackage{subfig}
      bs' <- crossRefBlocks bs
      return $ Pandoc meta bs'

genTemplate nameTemplate fileName = do
    let tFileName = "_build/temp/current.tpl"
    templateExist <- fileExists $ fileName <> ".tpl"
    mainTemplateString <- if templateExist
      then getTemplate $ fileName <> ".tpl"
      else pure $ T.pack $ Template.mainTemplate nameTemplate
    let templateString =
          T.replace "$commonTemplate$" Template.commonTemplate mainTemplateString
    liftIO $ do
      TIO.writeFile (tFileName <> ".0") templateString
      callCommand $ "sed -e 's/%.*$//g' " <> tFileName <> ".0 | cat -s > " <> tFileName
    runWithPartials $ PT.compileTemplate tFileName templateString

finishDoc template nameTemplate (_, topLevel) fileName citedPandoc = do
  runIO' $ do
    let citeMethod = case nameTemplate of
                       "snat" -> Natbib
                       "aas" -> Natbib
                       "revealjs" -> Citeproc
                       _ -> Biblatex
    (outExt,res0) :: (String,T.Text)<- case template of
      Right t -> case nameTemplate of
                   "revealjs" -> (,) ".html" <$> writeRevealJs (def{writerTemplate = Just t, writerTopLevelDivision = topLevel}) citedPandoc
                   _ -> (,) ".tex" <$> writeLaTeX (def{writerTemplate = Just t, writerTopLevelDivision = topLevel, writerCiteMethod = citeMethod}) citedPandoc
      Left e -> error e
    let res = removeHyperTarget res0
    liftIO $ TIO.writeFile ("_build/" <> fileName <> outExt) res
    compileLatex nameTemplate fileName
  where
    removeHyperTarget t = T.pack $ subRegex (mkRegex "^.hypertarget\\{fig:[^\\}]*\\}\\{%") (T.unpack t) "{%"
    compileLatex "revealjs" _ = pure ()
    compileLatex _ fileName = liftIO $ do
      callCommand $ unlines [ "cd _build"
                            , "./compileall.sh " <> fileName
--                          , "lualatex -interaction=nonstopmode " <> fileName <> ".tex"
--                          , "bibtex " <> fileName
--                          , "lualatex -interaction=nonstopmode " <> fileName <> ".tex"
--                          , "lualatex -interaction=nonstopmode " <> fileName <> ".tex"
                            , "cd .." ]
      putStrLn "======================"


doThemAll :: String -> Pandoc -> IO Pandoc
doThemAll nameTemplate (Pandoc mt blks0) = do
  blks1 <- flip walkM blks0
            $ includeScript
            >=> doBlockIO
            >=> doBlockIO
            >=> GoJS.includeGoJS
            >=> PlantUML.includePlantUMLBlock
            >=> Mermaid.doInclude
            >=> NU.processPegon nameTemplate
  blks12 <- flip walkM blks1 $ includeScriptImage >=> Chem.doIncludeImage
  blks1234 <- flip walkM blks12 $
            upgradeImageIO >=> doBlockIO
  blks <- walkM upgradeImageInline blks1234
  resP <- processCrossRef $ Pandoc mt blks
  doPandoc resP

processAcknowledgements :: Block -> Block
processAcknowledgements (Div (_,["facilities","show"],_) b) =
  Div nullAttr $ (RawBlock (Format "latex") $ T.unlines ["\\vspace{5mm}","\\facilities{"]) : b
              <> [ RawBlock (Format "latex") "}" ]
processAcknowledgements (Div (_,["software","show"],_) b) =
  Div nullAttr $ (RawBlock (Format "latex") $ T.unlines ["\\vspace{5mm}","\\software{"]) : b
              <> [ RawBlock (Format "latex") "}" ]
processAcknowledgements (Div (_,["acknowledgements","show"],_) b) =
  Div nullAttr $ (RawBlock (Format "latex") "\\begin{acknowledgements}") : b
              <> [ RawBlock (Format "latex") "\\end{acknowledgements}" ]
processAcknowledgements (Div (_,["appendix"],_) b) = Null
processAcknowledgements (Div (_,["dedicatory"],_) b) = Null
processAcknowledgements (Div (_,["acknowledgements"],_) b) = Null
processAcknowledgements (Div (_,"abstract":_,_) b) = Null
processAcknowledgements a = a

cleanVariable :: Block -> Block
cleanVariable (Div (_,["var",varName],_) _) = Null
cleanVariable (DefinitionList ((((Span (_,["notation"],_) _):_),_):_)) = Null
cleanVariable p = p

fillVariableB varMeta p@(Para [(Cite [c] _)])
  | T.isPrefixOf "var:" $ citationId c =
      let filler = join $ fmap (flip lookupMeta $ Meta varMeta) $ T.stripPrefix "var:" $ citationId c
       in case filler of
            Just (MetaBlocks s) -> Div nullAttr s
            _ -> Null
  | otherwise = p
fillVariableB _ p = p
fillVariableI varMeta p@(Cite [c] _)
  | T.isPrefixOf "var:" $ citationId c =
      let filler = join $ fmap (flip lookupMeta $ Meta varMeta) $ T.stripPrefix "var:" $ citationId c
       in case filler of
            Just (MetaBlocks s) -> Span nullAttr $ blocksToInlines s
            _ -> Str ""
  | otherwise = p
fillVariableI _ p = p

getNotation d@(DefinitionList ds@((((Span (_,["notation"],_) _):_),_):_)) =
  let symbolNotation symbolDef = ("symbol", MetaInlines symbolDef)
      defNotation listDef = zipWith (\a b -> (a,MetaBlocks b)) ["definition","unit","value","longdef"] listDef
      notation (s,ls) = MetaMap $ M.fromList $ (symbolNotation s):(defNotation ls)
   in map notation ds
getNotation _ = []

getVars (Div (_,["var",varName],_) bs) = Just (varName , MetaBlocks bs)
getVars (Div (_,v:a,_) bs)
  | elem v ["dedicatory","appendix","acknowledgements","software","facilities"] = Just (v, MetaBlocks bs)
  | v == "abstract" =
      let genLatexArgs :: Block -> Block
          genLatexArgs (Div a s) = Div a $ [RawBlock (Format "latex") "{"] ++ s ++ [RawBlock (Format "latex") "}"]
          genLatexArgs a = a
          val = case a of
                  -- \abstract{Context}{Aim}{Method}{Result}{Conclusion}
                  "fiveParts":_ -> [RawBlock (Format "latex") "\\abstract"] ++ (map genLatexArgs bs)
                  _ -> [RawBlock (Format "latex") "\\begin{abstract}"] ++ bs ++ [RawBlock (Format "latex") "\\end{abstract}"]
       in Just (v, MetaBlocks val)
  | otherwise = Nothing
getVars c = Nothing

walkM_ a b = () <$ walkM a b

doPandoc p = Diagrams.addPackagePGF =<< linkTex p

-----------------------------------------UpgradeImage----------------------------------------
upgradeImageInline :: Inline -> IO Inline
upgradeImageInline cb@(Image (l1,c,opts) caption (fileName, l2)) = do
  res <- runIO $ do
    latex0 <- writeLaTeX def $ Pandoc nullMeta [Plain [cb]]
    latex <- case T.lines latex0 of
              [] -> error $ "upgradeImageInline: writeLaTeX: latex0: " <> show cb
              [s] -> pure $ T.unlines [ "\\begin{figure}"
                                , "\\centering"
                                , s
                                , "\\caption{"
                                , "xxxCaptionxxx"
                                , "}"
                                , T.concat ["\\label{",l1,"}"]
                                , "\\end{figure}"
                                ]
              _ -> pure latex0
    let width = fromMaybe "1.0" $ lookup "size" opts
        includeSize = "includegraphics[keepaspectratio=true,width=" <> width <> "\\linewidth]{"
        containerSize = if elem "single" c then "" else ".49"
    let latex' = updateSize includeSize $ foldr (\fu fl -> fu c fl) latex [updateBegin containerSize, updateEnd, updateFullwidth]
    let (upperLatex,(_:lowerLatex)) = break (== "xxxCaptionxxx") $ T.lines latex'
    pure $ Span nullAttr
            $ concat [ [RawInline (Format "latex") $ T.unlines upperLatex]
                     , caption
                     , [RawInline (Format "latex") $ T.unlines lowerLatex]
                     ]
  case res of
    Left e -> error $ show e
    Right r -> pure r
  where
    updateEnd c l
      | elem "subfigure" c = T.replace "end{figure}" "end{subfigure}" l
      | otherwise = l
    updateBegin w c l
      | elem "subfigure" c =
          T.replace "begin{figure}" ("begin{subfigure}{"<>w<>"\\textwidth}") l
      | otherwise = l
    updateFullwidth c l
      | elem "fullwidth" c = T.replace "figure}" "figure*}" l
      | otherwise = l
    updateSize i l = T.replace "includegraphics{" i l
upgradeImageInline c = return c
upgradeImageIO :: Block -> IO Block
upgradeImageIO cb@(Para [Image (l1,c,opts) caption (fileName, l2)]) =
  walkM upgradeImageInline cb
upgradeImageIO c = return c

doBlockIO cb@(CodeBlock (_, classes, namevals) t)
  | "multiTable" `elem` classes = MultiMarkdown.doInclude cb
  | "chemfig" `elem` classes = Chem.doInclude cb
  | "feynmp" `elem` classes = FeynMP.doInclude cb
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
