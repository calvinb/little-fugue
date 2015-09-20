module Main where

import Euterpea
import Alto
import Tenor

main :: IO ()
main = play $ tempo (4/8) $ instrument ChurchOrgan parts where
    parts = Alto.alto :=: transpose (-12) Tenor.tenor
