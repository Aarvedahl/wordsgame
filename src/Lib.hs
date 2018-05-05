module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWord
    , findWordInLine
    , findWords
    , skew
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , cell2char
    , Cell(Cell, Indent)
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

data Cell = Cell (Integer, Integer) Char | Indent
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid(Integer, Integer)
coordsGrid =
  let cols = repeat [0..]
      rows = map repeat [0..]
  in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell-> IO()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Cell-> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

getLines :: Grid Cell -> [[Cell]]
getLines grid =
   let horizontal = grid
       vertical = transpose grid
       diagonal1 = diagonalize grid
       diagonal2 = diagonalize (map reverse grid)
       lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
   in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
-- diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word= undefined
 -- let lines = getLines grid
  --    found = or $ map (findWordInLine word) lines
  -- in if found then Just word else Nothing

findWords :: Grid Cell -> [String]  -> [[Cell]]
findWords grid words =
  let foundWords =  map (findWord grid) words
  in catMaybes foundWords


findWordInLine :: String -> [Cell]-> Maybe [Cell]
findWordInLine =  undefined -- isInfixOf


grid = [
         "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"]

languages = [ "BASIC"
                    , "COBOL"
                    , "CSHARP"
                    , "HASKELL"
                    , "LISP"
                    , "PHP"
                    , "PYTHON"
                    , "RUBY"
                    , "SCHEME"]

-- | stack build, stack exec words-exe
-- stack ghci
--- stack install