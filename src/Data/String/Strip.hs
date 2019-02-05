module Data.String.Strip (strip)  where

import Prelude (String, reverse, dropWhile)
import Prelude ((.))

import Data.Char

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

