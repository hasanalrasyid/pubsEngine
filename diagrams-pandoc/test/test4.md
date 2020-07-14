---
header-includes:
  \usepackage{chemfig}
---

## testnya

![Inilah pgf caption\label{pakelabel}](./images/floor.png){#fig:desc}

Here is a square:

~~~ {.diagram width=100 caption="this is a new caption"}
do
  let
    titles = ["Cash","Equity"]

    values :: [ (String,[Double]) ]
    values =
      [ ("Jun", [20,45])
      , ("Jul", [45,30])
      , ("Aug", [30,20])
      , ("Sep", [10,40])
      , ("Oct", [20,50])
      ]

    cb1 = do
          C.layout_title .= "Sample Bars"
          C.layout_title_style . C.font_size .= 10
          C.layout_x_axis . C.laxis_generate .= C.autoIndexAxis (map fst values)
          C.plot $ fmap C.plotBars $ C.bars titles (C.addIndexes (map snd values))
    cb = C.render (C.toRenderable (C.execEC cb1))(C._fo_size def)
  fontSelector <- C._fo_fonts def
  let env = C.createEnv C.vectorAlignmentFns 800 600 fontSelector
  let (res,_) = C.runBackend env cb
  return $ res :: IO (QDiagram PGF V2 Double Any)
~~~

