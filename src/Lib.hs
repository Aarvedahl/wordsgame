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
    , findWordInCellLinePrefix
    , Cell(Cell, Indent)
    , Game (gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , makeRandomGrid
    , formatGame
    , completed
    , fillInBlanks
    ) where

import System.IO
import Data.List (isInfixOf, transpose)
import Data.Char(toLower)
import Data.Maybe (catMaybes, listToMaybe)
import System.Random
import qualified Data.Map as M

data Game = Game { gameGrid  :: Grid Cell
                 , gameWords :: M.Map String (Maybe [Cell])
                 }
            deriving Show

data Cell = Cell (Integer, Integer) Char | Indent
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let grid'  = gridWithCoords grid
      words' = M.fromList $ map (\word -> (word, Nothing)) words
   in Game grid' words'

totalWords :: Game -> Int
totalWords game =  length . M.keys $ gameWords game

score :: Game -> Int
score game =  length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not (M.member word (gameWords game)) = game
playGame game word =
  let grid = gameGrid game
      foundWord = findWord grid word
      newGame = case foundWord of
        Nothing -> game
        Just cs ->
          let words = gameWords game
              words' = M.insert word foundWord words
          in Game grid words'
  in newGame

formatGame :: Game -> String
formatGame game = formatGameGrid game
    ++ "\n\n"
    ++ (show $ score game)
    ++ "/"
    ++ (show $ totalWords game)

makeRandomGrid gen =
  let (gen1, gen2)  = split gen
      row = randomRs('A', 'Z') gen1
  in row : makeRandomGrid gen2

fillInBlanks gen grid =
  let r = makeRandomGrid gen
      fill '_' r = r
      fill c _ =  c
  in zipOverGridWith fill grid r

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

formatGameGrid :: Game -> String
formatGameGrid game =
  let grid = gameGrid game
      dict = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell =
        let char = cell2char cell
        in if cell `elem` cellSet then char else toLower char
      charGrid = mapOverGrid formatCell grid
  in unlines charGrid

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
findWord grid word=
 let lines = getLines grid
     foundWords = map (findWordInLine word) lines
 in listToMaybe (catMaybes foundWords)


findWords :: Grid Cell -> [String]  -> [[Cell]]
findWords grid words =
  let foundWords =  map (findWord grid) words
  in catMaybes foundWords


findWordInLine :: String -> [Cell]-> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    Nothing -> findWordInLine word (tail line)
    cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe[Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
  = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing


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