---
header-includes:
  \usepackage{chemfig}
---

## testnya
Here is a square:

``` {.diagram}
example = square 10
main = defaultMain $ pad 1.1 example
```

~~~ {.diagram}
import Diagrams.TwoD.Vector         (perp)

-- Example using TeX primatives to make a text box with given width. Also
-- includes roundedRect background and labeling.

type D2 = Diagram PGF

main :: IO ()
main = defaultMain example

mytext :: D2
mytext = scale 2 . box 18 orange $ hboxPoint (sizedVBox 18 txt)
  where
    txt = "The sum of the squares of the lengths of the legs equals the square "
       ++ "of the length of the hypotenuse:"
       ++ "$$ a^2 + b^2 = c^2 .$$"

rightTriangle :: D2
rightTriangle
  = fromVertices [origin, mkP2 4 0, mkP2 4 3]
      # closeTrail
      # strokeTrail
      # centerXY
      # scale 12
      # fc dodgerblue
      # label (hboxPoint "\\chemfig{R-C-[::-60]O-[::-60]C-[::-60]R}") (V2 4 5)
      # label (hboxPoint "\\chemfig{*5(-=--=)}") (V2 1 1)

labeledTriangle :: D2
labeledTriangle = scale 5 $  rightTriangle
           # label (hboxPoint "$a$") unit_X
           # label (hboxPoint "$a$") unit_Y
           # label (hboxPoint "$a$") (V2 4 3)

example :: D2
example = frame 10 $ (|-|) labeledTriangle mytext

--
(|-|) :: D2 -> D2 -> D2
a |-| b = a ||| strutX 25 ||| b

box :: Double -> Colour Double -> D2 -> D2
box padding colour content
  = centerXY content <> roundedRect w h 2 # fc colour
  where
    V2 w h = (+padding) <$> size content


sizedVBox :: Double -> String -> String
sizedVBox w x = "\\hsize=" ++ show w ++ "em\\vbox{\\noindent " ++ x ++ "}"

label :: D2 -> V2 Double -> D2 -> D2
label l v a = besideWithGap 3 (perp v) a (centerXY l)

-- is there a better way to do this?
besideWithGap :: Double -> V2 Double -> D2 -> D2 -> D2
besideWithGap g v a b = beside v a b'
  where
    b' = beside v (strut (g *^ signorm v)) b


~~~

