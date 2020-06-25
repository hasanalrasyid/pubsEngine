#!/usr/bin/env stack
-- stack --resolver lts-11.3 script

{--
  -- stack --install-ghc runghc --resolver lts-11.3 --package text --package text-format
--}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

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


delta :: Bool -> b -> b -> b
delta x y z = if x then y else z

caller :: T.Text
caller = T.unlines  [ "callme with : genPDOSAtomicOrbital.hs [Atom:Orbital]                tailer    folder"
                    , "ex.         : genPDOSAtomicOrbital.hs 'O NiTd:2:3:4:5 CoTd:2:3:4:5' nico2o4   nico2o4.invB.0GGA"
                    ]
main1 = do
    kj@(wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    tampilkan kj


tampilkan [] = putStrLn "===================="
tampilkan (a:as) = do
        putStrLn $ show a
        putStrLn $ "++++"
        tampilkan as

main = do
    (jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
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
          zip  ctrlAtoms [1..nAtom]
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
        daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]

    let jdX = show $ xmin + 2
        jdY = show $ ymax + 4
        labelX = show $ xmin + 1
        labelY = show $ 0.7*ymax
        labelXr = "1"
        labelYr = show $ 0.8 * ymax
        labelEX = show $ ((xmax + xmin)*0.5) - 2.5
        labelEY = show $ foldr (*) 1 [ (-1), ymax, (*) 2.5 $ fromIntegral $ length aos]
        labelDOSX = show $ xmin - 2.25
        labelDOSY = show $ foldr (*) 1 [ (-1), ymax, (+) 1 $ fromIntegral $ length aos]

    let hasilTot' = if (wTot == "T") then intercalate "," $ map (susunTot foldernya tailer invStat ) [1,2] else ""
        hasilTot'' = if hasilTot' /= "" then hasilTot' else ""
        hasilTot  = insertLabel "Energy (eV)" (concat ["at ",labelEX,",",labelEY])
                  $ insertLabel "DOS (states/eV/unit-cell)" (concat ["rotate left at ",labelDOSX,",",labelDOSY])
                  $ insertLabel jd (concat ["at ", jdX,",", jdY])
                  $ insertLabel "Total" (concat ["at ",labelXr,",",labelYr," font 'Times New Roman Bold,10'"])
                  $ (++) "plot " hasilTot''

    let hasilSemua = hasilTot : (
              map (\((_,(_,a,_)) ,p) -> insertLabel (T.unpack $ T.replace "#" " " $ T.pack a) (concat ["at ",labelXr,",",labelYr," font 'Times New Roman Bold,10'"]) p)
              $ zip daftarCetak'
              $ map ((++) "plot ")
              $ map (intercalate ", ")
              $ chunksOf 2
              $ map (susunOrbs "dos" foldernya tailer invStat) daftarCetak
              )
    putStrLn $ concat
        $ (\a -> concat  [ (init a)
                         , [ "set format x '% h';"
                           , "set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out;"
                           , (last a)
                           ]
                         ]
          )
        $ map (insertText "unset label")
        $ map (insertLabel "spin-down" (concat ["at ",labelX,",-",labelY," font ',10'"]))
        $ map (insertLabel "spin-up" (concat ["at ",labelX,"+0.25,", labelY," font ',10'"])) hasilSemua


pSubPlot ( (_,(_,a,_)) ,p) = unlines [
    concat [ "set label '",a,"' at 3,15 font 'Times New Roman Bold,10'"]
  , unwords ["plot", p]
  ]

insertText :: String -> String -> String
insertText t p = unlines [t,p]

insertLabel :: String -> String -> String -> String
insertLabel l a p = unlines [ concat [ "set label '",l,"' ",a], p]

susunOrbs :: T.Text
                -> String
                -> String
                -> Int
                -> ((Int,((Int, Int, T.Text),String,[String])),Int) -- DaftarCetak
                -> String
susunOrbs job foldernya tailer invStat ((urutan,((jumlah,nomor,nama),judul,listOrbital)),spin) = unwords [
                                        Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u ($1*rydberg):((%s)*%d*(%d)*(%d)/rydberg ) w l ls %d notitle"
                                          (T.pack foldernya)
                                          job
                                          spin
                                          nomor
                                          (T.pack tailer)
                                          ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show [2..26] ))
                                          jumlah
                                          invStat
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          urutan
                                          ]


susunTot foldernya tailer invStat spin = unwords [
                                        Text.Printf.printf "'%s/dos.tot.%s' u ($1*rydberg):($%d *( %d ) *( %d ) / rydberg ) w l lc rgb 'black' notitle"
                                          (T.pack foldernya)
                                          (T.pack tailer)
                                          (delta (spin < 2) 2 (3) :: Int)
                                          invStat
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          ]



