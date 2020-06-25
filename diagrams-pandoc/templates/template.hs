#!/usr/bin/env stack
--stack --resolver lts-11.9 --install-ghc runghc --package diagrams-pandoc --stack-yaml xxxLocxxx/stack.yaml


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

--type D2 = Diagram PGF

--xx--
--b1ex = square 30 # lwG 0.05
--xx--

--main = defaultMain (pad 1.1 example)
--main = defaultMain example
--example = square 30 # lwG 0.05

-- Example using TeX primatives to make a text box with given width. Also
-- includes roundedRect background and labeling.

