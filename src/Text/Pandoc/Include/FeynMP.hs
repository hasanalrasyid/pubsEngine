{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.FeynMP (doInclude)
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
      let tex = BL.fromStrict $ UTF8.fromString $ unlines [ "\\documentclass{standalone}"
                        , "\\usepackage{amsmath}"
                        , "\\usepackage{feynmp-auto}"
                        , "\\begin{document}"
                        , "\\unitlength = 1mm"
                        , "\\begin{fmffile}{"++ mpHash ++ "}"
                        , "\\begin{math}"
                        , T.unpack mp
                        , "\\end{math}"
                        , "\\end{fmffile}"
                        , "\\end{document}"
                        ]
      isCompiled <- doesFileExist $ "_build/auto" </> mpHash <.> "pdf"
      B8.putStrLn tex
      if isCompiled then return ()
                    else inDir ("_build/temp" </> mpHash) $
                            \f -> do
                                    _ <- pipeProcess Nothing "pdflatex" [] tex
                                    _ <- pipeProcess Nothing "mpost" [f] ""
                                    _ <- pipeProcess Nothing "pdflatex" [] tex
                                    _ <- pipeProcess Nothing "mv"
                                          [ "texput.pdf"
                                          , "../auto" </> mpHash <.> "pdf"
                                          ] ""
                                    return ()
      return $ Div nullAttr
                $ [Para
                    [Image (label,[],opts)
                      [Str $ fromMaybe "FenymanDiagram" caption] (T.pack mpHash, label)
                    ]
                  ]

doInclude x = return x

