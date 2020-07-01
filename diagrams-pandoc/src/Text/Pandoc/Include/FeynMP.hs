{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.FeynMP (doInclude)
  where
--import Text.Pandoc.JSON
--import Text.Pandoc.Shared (addMetaField)
--import Text.Pandoc.Builder (fromList)
--import System.Process (callCommand)
--import Data.Maybe
--import System.FilePath.Posix (takeFileName)
import Text.Pandoc
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import Text.Pandoc.Include.Common
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Hashable
import Diagrams.Builder

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
      let mpHas' = 0 `hashWithSalt` mp
                     `hashWithSalt` "_build"
          mpHash = hashToHexStr mpHas'
          caption = lookup "caption" opts
          label = lookup "label" opts
      let out = "Figures" </> mpHash
      let tex = unlines [ "\\documentclass{article}"
                        , "\\usepackage{amsmath}"
                        , "\\usepackage{feynmp-auto}"
                        , "\\begin{document}"
                        , "\\pagestyle{empty}"
                        , "\\unitlength = 1mm"
                        , "\\begin{fmffile}{"++ mpHash ++ "}"
                        , mp
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
                    [Image (fromMaybe "" label, [], [])
                      [RawInline (Format "tex") $ fromMaybe "FenymanDiagram" caption] (out, "fig:")
                    ]
                  ]

doInclude x = return x

