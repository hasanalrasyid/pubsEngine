---
author: hasan
title: test
---
## testnya
Here is a square:


~~~{.diagram width=100}
let
    poly1 = polygon ( with & polyType  .~ PolyRegular 13 5
                           & polyOrient .~ OrientV )
    poly2 = polygon ( with & polyType  .~ PolyPolar (repeat (1/40 @@ turn))
                                                    (take 40 $ cycle [2,7,4,6]) )
 in (poly1 ||| strutX 1 ||| poly2)
~~~

~~~ {.inputTable file=images/table1.md }
~~~

here is picture
![testprint lah](images/testprint.ps)

~~~ {.inputTable file=images/table1.md }
Caption untuk table ini $x^2$ ada di sini. Caption ini akan mengoverride caption bawaan table**nya** sendiri.
~~~
