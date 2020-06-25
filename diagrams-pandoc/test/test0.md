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

~~~ {.diagram width=300 caption="saya baru"}
type D2 = Diagram PGF

main = onlineMain example

mytext :: OnlineTex D2
mytext = scale 2 . box 8 orange <$> hboxOnline (sizedVBox 18 txt)
  where
    txt = "The sum of the squares of the lengths of the legs equals the square "
       ++ "of the length of the hypotenuse:"
       ++ "$$ a^2 + b^2 = c^2 .$$"

rightTriangle
  = fromVertices [origin, mkP2 4 0, mkP2 4 3]
      # closeTrail
      # strokeTrail
      # centerXY
      # scale 12
      # fc dodgerblue
      # label (hboxPoint "\\chemfig{R-C-[::-60]O-[::-60]C-[::-60]R}") (V2 4 6)
      # label (hboxPoint "\\chemfig{*5(-=--=)}") (V2 1 1)

labeledTriangle :: OnlineTex D2
labeledTriangle = scale 5 <$> do
  a <- hboxOnline "$a$"
  b <- hboxOnline "$b$"
  c <- hboxOnline "$c$"

  pure $ rightTriangle
           # label a unit_X
           # label b unit_Y
           # label c (V2 4 3)

example = frame 10 <$> liftA2 (|-|) labeledTriangle mytext

a |-| b = a ||| strutX 25 ||| b

box padding colour content
  = centerXY content
 <> roundedRect w h 2
      # fc colour
  where
    V2 w h = (+padding) <$> size content

sizedVBox w x = "\\hsize=" ++ show w ++ "em\\vbox{\\noindent " ++ x ++ "}"

label l v a = besideWithGap 3 (perp v) a (centerXY l)

besideWithGap g v a b = beside v a b'
  where
    b' = beside v (strut (g *^ signorm v)) b
~~~

