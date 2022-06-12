{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Include.Chem (doInclude,doIncludeImage)
  where
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Hashable
import Diagrams.Builder (hashToHexStr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc
import Text.Pandoc.Include.Common (mdOption)
import Text.Pandoc.Process
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Include.Utils
import Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Text.RawString.QQ
import Data.FileEmbed

withDir :: FilePath -> IO a -> IO a
withDir path f = do
  dir <- getCurrentDirectory
  setCurrentDirectory path
  result <- f
  setCurrentDirectory dir
  return result

inDir :: FilePath -> (FilePath -> IO a) -> IO a
inDir path f = do
  let (dir, file) = splitFileName path
  withDir dir $ f file

doIncludeImage (Image (label, ("chemfig":classes), opts) caption _) = do
  mp <- T.readFile $ T.unpack $ fromMaybe "chemfigSourceFile" $ lookup "src" opts
  T.putStrLn mp
  let mpHash = getHash mp
  let tex = BL.fromStrict $ blankLatex <> (UTF8.fromString $ unlines [ T.unpack mp, "\\end{document}" ])
  isCompiled <- doesFileExist $ "_build/auto" </> mpHash <.> "pdf"
  B8.putStrLn tex
  B8.putStrLn $ BL.fromStrict $ UTF8.fromString mpHash
  if isCompiled then return ()
                else do
                  callCommand "mkdir -p _build/temp"
                  BL.writeFile ("_build/temp" </> mpHash <> ".tex") tex
                  callCommand $ unwords ["lualatex", "--output-directory=_build/temp", "_build/temp"</>mpHash<>".tex"]
                  callCommand $ unwords ["lualatex", "--output-directory=_build/temp", "_build/temp"</>mpHash<>".tex"]
                  callCommand $ "mv _build/temp" </> mpHash <>".pdf _build/auto" </> mpHash <> ".pdf"
  return $ Image (label,classes,opts)
                  caption (T.pack mpHash, label)
doIncludeImage c = pure c

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (label, classes@("chemfig":_), opts) mp) = do
      caption0 <- runIO $ readMarkdown mdOption $ fromMaybe "" $ lookup "caption" opts
      let caption = case caption0 of
                      Left _ -> []
                      Right (Pandoc _ c) -> blocksToInlines c
      img <- doIncludeImage (Image (label, classes, opts) caption ("",label))
      return $ Div nullAttr
                $ [Para [img]]
doInclude cb = walkM doIncludeImage cb

blankLatex = $(embedFile "embed/blankChemFig.text")

