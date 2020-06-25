
~~~{#fig:dia1 .diagram width=300}
do
  let pS = text "$\\Sigma$" <> circle 1
      pG = text "$G$" <> circle 1
      pV = text "$\\Gamma$" <> circle 1
      p1 = text "$1$" <> circle 1
      pP = text "$P$" <> circle 1
      pW = text "$W$" <> circle 1
      --pPsi = text "$\\sum_{nk}\\frac{\\psi_{nk}(r)\\psi_{nk}^*(r')}{\\omega - \\epsilon_{nk}\\pm i\\eta}$"
      --pPsi = text "$\\sum_{nk}$" <> circle 1
      qsGW = (atPoints (trailVertices $ pentagon 4) $ zipWith (\a b -> a # named (show b)) [ pG, pS, pW, pP, p1 ] [1..])
            # (applyAll $ map (\(a,b) -> connectOutside (show a) (show b)) $  (2,5):[ (j,k) | j <- [3..5] :: [Int], let k = j - 1 ])
      scGW = (atPoints (trailVertices $ pentagon 4) $ zipWith (\a b -> a # named (show b)) [ pG, pS, pW, pP, pV ] [1..])
            # (applyAll $ map (\(a,b) -> connectOutside (show a) (show b)) $  (1,5):[ (j,k) | j <- [2..5] :: [Int], let k = j - 1 ])
  return $ scGW ||| strutX 2 ||| qsGW
~~~

~~~
    pV = text "$\\Gamma$" <> circle 1
~~~

ini test @fig:dia1
