{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

{-# LANGUAGE ApplicativeDo, TemplateHaskell, CPP, OverloadedStrings #-}

module Text.Pandoc.Include.CrossRef (doCrossRef)
  where

import Text.Pandoc
import Text.Pandoc.JSON

import Text.Pandoc.CrossRef
import Control.Monad
import System.IO hiding (putStrLn)
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import ManData

import Prelude hiding (putStrLn)
import Data.Semigroup ((<>))

man, manHtml :: T.Text
man = "Man Text"
manHtml = "Man HTML"

data Flag = NumericVersion | Version | Man | HtmlMan

doCrossRef :: Pandoc -> IO Pandoc
doCrossRef p@(Pandoc meta _) =
      runCrossRefIO meta (Just $ Format "latex") defaultCrossRefAction p

  {-
run :: Parser (IO ())
run = do
  man' <- flag Nothing (Just Man) (long "man" <> help "Show manpage")
  hman <- flag Nothing (Just HtmlMan) (long "man-html" <> help "Show html manpage")
  fmt <- optional $ strArgument (metavar "FORMAT")
  --return $ go (numVers <|> vers <|> man' <|> hman) fmt
  return $ go ( man' <|> hman) fmt
  where
    go :: Maybe Flag -> Maybe String -> IO ()
    go (Just Man    ) _ = T.putStrLn "no man"
    go (Just HtmlMan) _ = T.putStrLn "no htmlman"
    go Nothing _ = toJSONFilter f
    f fmt p@(Pandoc meta _) =
      runCrossRefIO meta fmt defaultCrossRefAction p

-}
{-
main :: IO ()
main = join $ execParser opts
  where
    opts = info (run <**> helper)
      (  fullDesc
      <> O.header "pandoc-crossref - Pandoc filter for cross-references"
      )

-}

