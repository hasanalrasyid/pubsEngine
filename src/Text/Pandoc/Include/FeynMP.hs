{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.FeynMP (doInclude)
  where
--import Text.Pandoc
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Hashable
import Diagrams.Builder
import qualified Data.Text as T

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
doInclude (CodeBlock (_, classes, opts) mp)
  | "feynmp" `elem` classes = do
      let (mpHas' :: Int) = hashWithSalt 0 $ "_build/auto" <> mp
          mpHash = hashToHexStr mpHas'
          caption = lookup "caption" opts
      let out = "_build/auto" </> mpHash
      let tex = unlines [ "\\documentclass{article}"
                        , "\\usepackage{amsmath}"
                        , "\\usepackage{feynmp-auto}"
                        , "\\begin{document}"
                        , "\\pagestyle{empty}"
                        , "\\unitlength = 1mm"
                        , "\\begin{fmffile}{"++ mpHash ++ "}"
                        , T.unpack mp
                        , "\\end{fmffile}"
                        , "\\end{document}"
                        ]
      isCompiled <- doesFileExist $ out <.> "pdf"
      if isCompiled then return ()
                    else inDir ("_build" </> mpHash) $
                            \f -> do
                                    _ <- readProcess "xelatex" [] tex
                                    _ <- readProcess "mpost" [f] []
                                    _ <- readProcess "xelatex" [] tex
                                    _ <- readProcess "pdfcrop" ["texput.pdf"] []
                                    _ <- readProcess "mv" [ "texput-crop.pdf"
                                                          , ".." </> out <.> "pdf"
                                                          ] []
                                    return ()
      return $ Div nullAttr
                $ [Para
                    [Image nullAttr
                      [Str $ fromMaybe "FenymanDiagram" caption] (T.pack out, "fig:")
                    ]
                  ]

doInclude x = return x

