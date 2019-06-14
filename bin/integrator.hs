#!/usr/bin/env stack
-- stack --resolver lts-11.3 script

{--
  -- stack --install-ghc runghc --resolver lts-11.3 --package text --package text-format
--}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

--import Turtle                       --
--import Turtle.Helper
import qualified Control.Foldl as Fold
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Format as T
import Text.Printf
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either
-------------------------

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel (readMatrix)
import Numeric.LinearAlgebra.Data hiding (find)
import Data.Char
import System.Process
import System.IO
-------------------------
import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
import qualified Text.PrettyPrint.Boxes as TB
import Data.List
-------------------------
import Text.Pandoc
import Control.Monad ((<=<))
-- ===============================================
-- start of Accelerate
-- import Data.Array.Accelerate              as A
-- import Data.Array.Accelerate.LLVM.Native  as CPU
-- import Data.Array.Accelerate.LLVM.PTX     as GPU
--
{-
dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

main1 = do
    kj@(texFile:jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    putStrLn $ show kj
--    let xs = fromList (Z:.10) [0..]   :: Vector Float
--    let ys = fromList (Z:.10) [1,3..] :: Vector Float
--    CPU.run $ dotp (use xs) (use ys)
-}

{--
pad width x = x ++ replicate k ' '
  where k = width - length x

fmt_column :: [String] -> Box
fmt_column items = vcat left (addHead $ map (text.pad width) items)
  where width = maximum $ map length items
        hsep = text ( replicate width '-' )
        addHead (a:as) = a:hsep:as

table :: [[String]] -> Box
table rs = vsep TB.<> hcat top (intersperse vsep (map fmt_column columns)) TB.<> vsep
  where

    columns = transpose rs
    nrows = length rs
    vsep =  vcat left $ map char ("|" ++ (concat $ replicate nrows "|"))

delta :: Bool -> b -> b -> b
delta x y z = if x then y else z

caller :: T.Text
caller = T.unlines  [ "callme with : genPDOSAtomicOrbital.hs [Atom:Orbital]                tailer    folder"
                    , "ex.         : genPDOSAtomicOrbital.hs 'O NiTd:2:3:4:5 CoTd:2:3:4:5' nico2o4   nico2o4.invB.0GGA"
                    ]
tampilkan [] = putStrLn "===================="
tampilkan (a:as) = do
        putStrLn $ show a
        putStrLn $ "++++"
        tampilkan as

getIntegralDOS = id
-}

main1 = do
  (fxY:_) <- getArgs
  xY <- loadMatrix fxY
  let intgTot = integrateUntilEf 0 $ toLists xY
  putStrLn $ show intgTot

f :: Double -> Double
f x = sin x

main = do
  putStrLn $ show $ take 100 $ map (\x -> [x/100,sin (pi*x/100)]) $ [1..]
  putStrLn $ show $ sin $ 2*pi
--  putStrLn $ show $ fromLists $ map (\x -> [x,sin x]) $ [0,1..]


{-
main = do
    (texFile:jd:jdHead:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
    -------------------------------start calculation------------------------
      -------------------------------generating data------------------------
    let invStat = read invS :: Int
    let ymax = read ymax' :: Double
        [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
    let ctrlAtoms =
          catMaybes $
          map ( T.stripPrefix "ATOM=" .  head) $
          filter (/=[]) $
          map ( T.words . T.takeWhile (/='#') ) $
          head $
          splitWhen (T.isPrefixOf "SPEC") $
          last $ splitWhen (T.isPrefixOf "SITE")
          $ T.lines fCtrl
        nAtom = length ctrlAtoms
    -- uniqAtoms : [(jumlah,nourutAtom,symbol)]
    let uniqAtoms =
          map (\a -> (length a, snd $ head a, fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  ctrlAtoms [1..]
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
        daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]
      -------------------------------generating DOS data------------------------
    totalDOS <- loadMatrix $ foldernya ++ "/dos.tot." ++ tailer
      -------------------------------integrating DOS data------------ddu------------
    let intgTot = map (\ii -> integrateUntilEf 0
                              $ (++ ([toList $ getY0 ii])) $ takeWhile (\(a:_) -> a <= 0)
                              $ toLists ii) $ map (\i -> totalDOS ¿ [0,i]) [1,2] -- run it on spin [1,2]
    --putStrLn $ show intgTot

      -------------------------------generating PDOS data------------------------
    let aoSet = map ( (\(a:l:as) -> (a,l,map ( ((+) (-1)) . read :: String -> Int) as) ) . splitOn ":") aos
    pdosA <- sequence $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x])
              -- ((namaAtom,jdAtom,[intAOs]),[(nourutAtom,namaAtom)])
              -- map ditambah -1 karena input mengikuti gnuplot
              -- input : d kolom 6-10
              -- gnuplot : d kolom 6-10
              -- hmatrix : d kolom 5-9
              $ map (\x@((_,i):_) -> (head $ takeAO i aoSet ,x) )
              $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
              $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms
      -------------------------------integrating PDOS data------------------------
    let intgPdosA =  map (\(s,j,mP) -> (s,j,integrateUntilEf 0
                                            $ (++ ([toList $ getY0 mP])) $ takeWhile (\(a:_) -> a <= 0)
                                            $ toLists mP)) pdosA
      -------------------------------generating MarkDown Representation------------------------
    let rIntgAll' =
          render $ table
          $ (:) (splitOn "|" jdHead)
          $ (:) ((:) "Total" $ map showDouble $ intgTot)
          $ map (\[(_,_,u),(_,j,d)] ->  j : map showDouble [u,d])
          $ groupBy (\(_,a,_) (_,b,_) -> a == b)
          $ sortBy (\(_,a,_) (_,b,_) ->  compare a b ) intgPdosA
    let rIntgAll = unlines  [
                            rIntgAll'
                            , "Table: " ++ jd
                            ]
    putStrLn rIntgAll
      -------------------------------generating LaTex Representation------------------------
    resIntAll' <- runIOorExplode $
      ((writeLaTeX def) <=< (readMarkdown def{
        readerExtensions = foldr enableExtension multimarkdownExtensions [Ext_tex_math_dollars, Ext_raw_tex, Ext_table_captions]
                                             }))
      $ T.pack $ rIntgAll
    let resIntAll = unlines [
                            "\\begin{longtable}[]{@{}lSS@{}}"
                            , unlines $ tail $ lines $ T.unpack resIntAll'
                            ]
    T.writeFile texFile $ T.pack resIntAll
      where
        getAllPDOS (s,n,pd) = do
          let pdTot = getY0 pd
          return (s,n,pdTot)
        takeAO i [] = []
        takeAO i (a@(n,_,_):as) = if (i == n) then [a]
                                     else takeAO i as

-}

integrateAll res ([enA,nA]:b@[enB,nB]:as)
  | as == [] = (res + (enB - enA)*(nA+nB)*0.5)
  | otherwise = integrateUntilEf (res + (enB - enA)*(nA+nB)*0.5) (b:as)

integrateUntilEf res [] = res
integrateUntilEf res ([enA,nA]:b@[enB,nB]:as)
  | as == [] = integrateUntilEf (res + (enB - enA)*(nA+nB)*0.5) []
  | enB <= 0 = integrateUntilEf (res + (enB - enA)*(nA+nB)*0.5) (b:as)
  | otherwise = integrateUntilEf (res + (enB - enA)*(nA+nB)*0.5) []

{-
showDouble 0 = show 0
showDouble x = printf "%0.3f" x

hashSpaceText t = T.unpack $ T.replace "#" " " $ T.pack t


pdosA' :: String -> String -> [((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)]
pdosA' foldernya tailer = fmap (getPDOS foldernya tailer) [1,2]
---------------------------------------------------
sumRow a = a #> konst 1 (cols a)
getPDOS' res _ _ []  = return res
getPDOS' res tmpf intAOs (nf:nfiles)  = do
  _ <- system $ "mkdir -p temp; more +2 " ++ nf ++ " > " ++ tmpf
  aoDOS' <- fmap (\x -> (¿) x intAOs) $ loadMatrix tmpf
  getPDOS' (fromBlocks [[res,aoDOS']]) tmpf intAOs nfiles
-------------------------------------------------------------

getPDOS :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
getPDOS theFolder tailing spin (a@(namaAtom,jdAtom,intAOs),lsAtoms) = do
  let namaFaos = map (\(x,_) -> theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (printf "%03d" x) ++ "." ++ tailing) lsAtoms
  (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
  hClose h
  _ <- system $ "mkdir -p temp; more +2 " ++ (head namaFaos) ++ " > " ++ tmpfile
  aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix tmpfile
  let zeroE = asColumn $ konst 0 (rows aoE)
  pDOS <- fmap sumRow $ getPDOS' zeroE tmpfile intAOs namaFaos
  return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, asColumn pDOS]])
------------------------------------------------------------------

getY0 dos = getY0' lowPos higNeg
  where
    rTDOS = toRows $ dos
    highestNeg = (+) (-1) $ fromJust $ findIndex (\a -> (atIndex a 0) >= 0) rTDOS
    lowestPos = highestNeg + 1
    higNeg = rTDOS !! highestNeg
    lowPos = rTDOS !! lowestPos

getY0' a b = a + (scale m v)
  where
    v = b - a
    m = ((*) (-1) $ a ! 0) / ((b ! 0) - (a ! 0))


--}
