---
header-includes:
  \usepackage{chemfig}
---

## testnya

![Inilah pgf caption\label{pakelabel}](./images/floor.png){#fig:desc}

Here is a square:

~~~ {.diagram width=100 caption="this is a new caption"}
let  example = square 10
     t = pad 1.1 example
 in t
~~~

