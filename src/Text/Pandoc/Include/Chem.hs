{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Include.Chem (doInclude)
  where
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Hashable
import Diagrams.Builder (hashToHexStr)
import qualified Data.Text as T
import Text.Pandoc.Process
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

doInclude :: Block -> IO Block
doInclude (CodeBlock (label, classes, opts) mp)
  | "chemfig" `elem` classes = do
      let (mpHas' :: Int) = hashWithSalt 0 $ "_build/auto" <> mp
          mpHash = hashToHexStr mpHas'
          caption = lookup "caption" opts
      let tex = BL.fromStrict $ blankLatex <> (UTF8.fromString $ unlines [ T.unpack mp, "\\end{document}" ])
      isCompiled <- doesFileExist $ "_build/auto" </> mpHash <.> "pdf"
      B8.putStrLn tex
      B8.putStrLn $ BL.fromStrict $ UTF8.fromString mpHash
      if isCompiled then return ()
                    else do
                      callCommand "mkdir -p _build/temp"
                      BL.writeFile ("_build/temp" </> mpHash <> ".tex") tex
                      callCommand $ unwords ["lualatex -interaction=nonstopmode", "--output-directory=_build/temp", "_build/temp"</>mpHash<>".tex"]
                      callCommand $ unwords ["lualatex -interaction=nonstopmode", "--output-directory=_build/temp", "_build/temp"</>mpHash<>".tex"]
                      callCommand $ "pdfcrop _build/temp" </> mpHash <>".pdf _build/auto" </> mpHash <> ".pdf"
      return $ Div nullAttr
                $ [Para
                    [Image (label,classes,opts)
                      [Str $ fromMaybe "ChemFig module" caption] (T.pack mpHash, label)
                    ]
                  ]

doInclude x = return x

blankLatex = $(embedFile "embed/blankChemFig.text")

