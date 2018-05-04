module Main where

import Lib

main :: IO ()
main =
  let gwc = gridWithCoords grid
  in outputGrid grid
