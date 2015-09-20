module Fugue where

import Euterpea
import Alto
import Tenor

main :: IO ()
main = play $ tempo (4/7) $ instrument ChurchOrgan Alto.alto
