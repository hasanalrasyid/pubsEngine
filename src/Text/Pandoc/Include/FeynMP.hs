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
import Text.Pandoc.Process
import Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL

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
  | "feynmp" `elem` classes = do
      let (mpHas' :: Int) = hashWithSalt 0 $ "_build/auto" <> mp
          mpHash = hashToHexStr mpHas'
          caption = lookup "caption" opts
      let tex = BL.fromStrict $ UTF8.fromString $ unlines [ "\\documentclass{article}"
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
      isCompiled <- doesFileExist $ "_build/auto" </> mpHash <.> "pdf"
      if isCompiled then return ()
                    else inDir ("_build/temp" </> mpHash) $
                            \f -> do
                                    _ <- pipeProcess Nothing "xelatex" [] tex
                                    _ <- pipeProcess Nothing "mpost" [f] ""
                                    _ <- pipeProcess Nothing "xelatex" [] tex
                                    _ <- pipeProcess Nothing "pdfcrop" ["texput.pdf"] ""
                                    _ <- pipeProcess Nothing "mv"
                                          [ "texput-crop.pdf"
                                          , "../auto" </> mpHash <.> "pdf"
                                          ] ""
                                    return ()
      return $ Div nullAttr
                $ [Para
                    [Image (label,[],opts)
                      [Str $ fromMaybe "FenymanDiagram" caption] (T.pack mpHash, "fig:")
                    ]
                  ]

doInclude x = return x

