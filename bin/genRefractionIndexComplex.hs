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

defAtHeaders = [ "H   1 1  1.00 0.800   1.000   0.80  0.80  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "C   6 4  4.0  1.0    12.0000  1.33  1.33  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "N   7 2  5.0  1.0    14.007   1.3   1.3   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "O   8 6  6.00 1.000  15.999   1.3   1.3   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Mg 12 6  8.00 1.000  24.305   1.7   1.7   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Si 14 8  4.00 1.0    28.086   1.75  1.75  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Ar 18 1  8.00 1.000  39.948   1.4   1.4   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Co 27 2 17.0  1.0    58.9332  2.2   2.2   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Sb 51 2 15.00 1.000 121.75000 2.500 2.500 NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Ba 56 7 10.00 1.000 137.327   2.6   2.6   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Pt 78 1 10.0  1.0   195.09    2.32  2.32  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Au 79 1 11.0  1.0   196.9665  2.7   2.7   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Ti 81 2 13.0  1.0   204.37    2.5   2.5   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Pb 82 4 14.0  1.0   207.2     2.5   2.5   NZA NA ZV RCMAX PMASS RATS RATS1"
               ]


