module Main where

import Euterpea
import Alto
import Tenor

main :: IO ()
main = play $ tempo (4/7) parts where
    alto' = instrument ChurchOrgan Alto.alto
    tenor' = instrument ChurchOrgan Tenor.tenor
    parts = alto' :=: transpose (-12) tenor'
