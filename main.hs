module Main where

import Euterpea
import Alto
import Tenor

main :: IO ()
main = play $ tempo (4/8) $ parts where
    alto'  = instrument ChurchOrgan Alto.alto
    tenor' = instrument ChurchOrgan Tenor.tenor
    parts  = alto' :=: tenor'
