#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-11.3 --package fortran-src

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Main where

import           Options.Applicative
import Data.Semigroup
import Data.Maybe (fromJust,fromMaybe)
import Language.Fortran.Parser.Utils (readReal)

import System.Directory (doesFileExist)
import System.IO (openTempFile,hClose)
import qualified System.Process as SP
import Numeric.LinearAlgebra.Data hiding ((<>))
import Numeric.LinearAlgebra hiding ((<>))

main :: IO ()
main = do
    opts <- execParser withHelp
    bOK <- doesFileExist $ _inputEPSnolfc opts
    if bOK then genRefIndex opts
           else putStrLn "Error... input needed"

w_eps_FromLine l = let (_:_:_:w:er:ei:_) = map (fromJust . readReal) $ words l
                          in (w,er,ei)

c = 137 --speed of light (a.u)
eV2hartree = 1/27.211386

genRI (w,er,ei) = let r = sqrt (er :+ ei)
                      wHartree = w * eV2hartree
                      a = 2 * wHartree * (imagPart r)/c
                         in [w,er,ei,realPart r,imagPart r,a]

genRefIndex :: Opts -> IO ()
genRefIndex opts = do
  let fEPS' = _inputEPSnolfc opts
  let fOut = _outDir opts
  _ <- SP.system $ "mkdir -p temp"
  (tmpfile,h) <- openTempFile "temp" "EPS.temp"
  hClose h
  _ <- SP.system $ unwords ["more +2", fEPS', "| sed -e 's/D/E/g' >", tmpfile]
  fEPS <- readFile tmpfile
  let eps = lines fEPS
      res = map (genRI . w_eps_FromLine) eps
  saveMatrix fOut "%.12f" $ fromLists res

data Opts = Opts {
    _outDir       :: FilePath,
    _inputEPSnolfc :: FilePath
                 } deriving Show

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption (help "target output" <> short 'o' <> value "out.dat")
             <*> strOption (long "input-epsnolfc" <> short 'i' <> metavar "EPSnoLFC"
                            <> help "file EPS*.nolfc.dat from ecalj run")

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "Generating Refractive Index Complex from Dielectric, output format in [w,er,ei,realPart r,imagPart r,a]"
       <> header "ecalj: Refractive Index from Dielectric Function using n + ik = sqrt eps")
