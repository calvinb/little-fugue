module Fugue where

import Euterpea
import Alto

main :: IO ()
main = play $ tempo (4/7) $ instrument ChurchOrgan Alto.alto
