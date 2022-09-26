{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Script where

import qualified Text.Pandoc.Include.Table as InTable
import Text.Pandoc.Include.Thesis (linkTex)
import qualified Text.Pandoc.Include.Markdown as Markdown
import qualified Text.Pandoc.Include.MultiMarkdown as MultiMarkdown
import qualified Text.Pandoc.Include.Diagrams as Diagrams
import qualified Text.Pandoc.Include.Delegate as Delegate
import qualified Text.Pandoc.Include.FeynMP as FeynMP
import           Text.Pandoc.JSON
import Text.Pandoc.Walk
import qualified Text.Pandoc.Class as PIO
import qualified Text.Pandoc.Templates as PT

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)

import Data.Maybe
import System.Environment (getArgs)

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
import Text.Pandoc.Shared -- blocksToInlines

saveLibrary :: [T.Text] -> FilePath -> String -> IO [Block]
saveLibrary (_:lib:_) fileName script = do
  let libName = T.unpack lib
  writeFile ("_build/temp/lib/" </> libName </> fileName <.> libName) script
  let (cmd,description,_) = getCommand libName []
  r <- readProcess cmd [] $ unlines [script, description]
  res <- runIO $ readMarkdown mdOption $ T.pack r
  return $ case res of
      Left e -> [Para [Str $ "ERROR: saveLibrary: readMarkdown: " <> (T.pack $ show e)]]
      Right (Pandoc _ b) -> b

getCommand c@"py"      files = ( "python","print(description)"
                               , unlines [ "import sys, os"
                                         , unlines $ map (\f -> unwords ["from lib.py." <> f,"import *"] ) $ map takeBaseName files
                                         ]
                               )
getCommand c@"sh"      files = ("zsh","echo description"
                               ,  unlines $ map (\f -> ". '_build/temp/lib"</> c </> f <> "'") files )
getCommand c@"gnuplot" files = ("gnuplot",unlines ["set print '-'", "print description"]
                               ,  unlines $ map (\f -> "load '_build/temp/lib"</> c </> f <> "'") files )
getCommand a         _ = ("zsh","echo ERROR: getDescription: saveLibrary: do not know how to call the library for class: " <> a, "")

writeScriptResult res (_, ["script",cmd,"md"], _) = do
  r <- runIO $ readMarkdown mdOption $ T.pack res
  return $ Div nullAttr $ case r of
    Left e -> [Para [Str $ "ERROR: script."<> cmd <>".md: cannot parse the markdown output: " <> (T.pack $ show e)]]
    Right (Pandoc _ b) -> b
writeScriptResult _ (label, ["script",c,"img"], opts) = do
  let fileName = fromMaybe "defaultImg" $ lookup "file" opts
  ltx <- runIO $ do
    let c = fromMaybe "" $ lookup "caption" opts
    (readMarkdown mdOption $ c) >>= writeLaTeX def
  let caption = case ltx of
                  Left e -> [Str $ "ERROR: script."<> c <>".md: cannot parse the markdown output: " <> (T.pack $ show e)]
                  Right p -> [RawInline (Format "latex") p]
  return $ Div nullAttr [Para [Image (label,[],opts) caption (fileName, label) ]]
writeScriptResult res c = return $ Div nullAttr [Para [Str $ T.pack $ "ERROR: unacceptable script class headers, we got: " <> show c ]]

extractSource text opts0 =
  case lookup "src" opts0 of
    Just a -> do
      t <- TIO.readFile $ T.unpack a
      let o = M.toList $ M.alter (\_ -> Just text) "caption" $ M.fromList opts0
      return (t,o)
    _ -> return (text,opts0)

includeScript :: Block -> IO Block
-----------------------------------------Library----------------------------------------
includeScript cb@(CodeBlock (label, classes@("script":_:"lib":_), opts0) text) = do
  (script,opts) <- extractSource text opts0
  let fileName = T.unpack $ fromMaybe "libDefault" $ lookup "file" opts
  let upperPart = if elem "show" classes then [CodeBlock (label,[],[]) $ T.pack $ unlines $ deleteBy isPrefixOf "description"  $ lines $ T.unpack script]
                                         else []
  lowerPart <- saveLibrary classes fileName $ T.unpack script
  return $ Div nullAttr $ upperPart <> lowerPart

-----------------------------------------Program----------------------------------------
includeScript cb@(CodeBlock (label, a@["script",c,outType], opts0) text) = do
  (script,opts) <- extractSource text opts0
  putStrLn $ "===========" <> show opts
  let command = T.unpack c
  files <- listDirectory $ "_build/temp/lib/" <> command
  let (cmd,_,header) = getCommand command files
      s = unlines [header,T.unpack script]
  putStrLn s
  res <- readProcess cmd [] s
  writeScriptResult res (label,a,opts)
includeScript cb@(CodeBlock (a, ("script":_), opts) t) =
  includeScript $ CodeBlock (a,["script","py","md"], opts) t
includeScript cb@(Div (a, c@("multiImage":_), opts) t) = do
  let images0 = query getImage cb
      caption = blocksToInlines $ walk notImage t
      fullwidth = if elem "fullwidth" c then "*" else ""
  images <- walkM includeScriptImage images0
  let bigImage = Image (a,[],opts) caption ("xxxdummyxxx",a)
  res <- runIO $ do
    bigImage0 <- writeLaTeX def $ Pandoc nullMeta [Para [bigImage]]
    let bigImage = if elem "fullwidth" c then T.replace "{figure}" "{figure*}" bigImage0
                                         else bigImage0
    let (upperImage,(_:lowerImage)) = break (T.isInfixOf "xxxdummyxxx") $ T.lines bigImage
    let r1 = Div (a,[],opts) $ concat [ [RawBlock (Format "latex") $ T.unlines upperImage]
                                      , [Plain $ walk setSubfigure images ]
                                      , [RawBlock (Format "latex") $ T.unlines lowerImage]
                                      ]
    pure r1
  return $ case res of
    Right p -> p
    Left e -> error $ show e
  where
    setSubfigure (Image (l,a,o) c (f,_)) = Image (l,"subfigure":a,o) c (f,l)
    setSubfigure c = c
    getImage :: Inline -> [Inline]
    getImage i@(Image _ _ _) = [i]
    getImage _ = []
    notImage (Image _ _ _) = Space
    notImage c = c

includeScript cb = return cb


includeScriptImage img@(Image (label,("script":c:a),opts0) caption (fileName, _)) =
  case lookup "src" opts0 of
    Nothing -> pure img
    Just srcFile -> do
      (script,opts) <- extractSource "" opts0
      let command = T.unpack c
      files <- listDirectory $ "_build/temp/lib/" <> command
      let (cmd,_,header) = getCommand command files
          s = unlines [header,T.unpack script]
      putStrLn s
      TIO.writeFile "_build/temp/image.gnuplot" $ T.pack s
      case cmd of
         "gnuplot" -> doGnuplot s $ T.unpack fileName
         "python" -> do
           let f = T.unpack fileName
           TIO.writeFile ("_build/temp/"</>f<.>"py") $ T.pack s
           let params = map T.unpack $ fromMaybe [] $ T.words <$> lookup "args" opts
           _ <- readProcess cmd (("_build/temp/"<>f<>".py"):params) ""
           isTex <- doesFileExist $ "_build/temp"</>f<.>"tex"
           case isTex of
             True -> do
               callCommand $ "latex --output-directory=_build/temp _build/temp"</> f<.>"tex"
               callCommand $ "dvips -E _build/temp/"</>f<>".dvi -o _build/auto" </> f<.> "eps"
             _ -> pure ()
         _ -> error $ "includeScriptImage: unknown cmd: " <> cmd <> " : " <> show img

      return $ Image (label,a,opts) caption (fileName, label)
includeScriptImage img = pure img

doGnuplot s fileName = do
  callCommand $ "gnuplot _build/temp/image.gnuplot"
  callCommand $ "latex --output-directory=_build/temp _build/temp"</> fileName <>".tex"
  callCommand $ "dvips -E _build/temp/"</>fileName<>".dvi -o _build/auto" </> fileName <> ".eps"

