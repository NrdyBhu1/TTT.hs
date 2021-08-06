import Data.Foldable
import Data.Sequence hiding (replicate)
import System.Random (randomRIO)
import System.Exit (exitSuccess)

type Board = [Row]
type Row = [Move]
data Move = X | O | Blank
    deriving (Eq)

instance Show Move where
    show Blank = " "
    show X = "X"
    show O = "O"

data Game = Game
  {
      move :: Move,
      board :: Board
  }
  deriving Show

getRow :: Int -> Int
getRow pos
    | pos < 3 = 0
    | pos >= 3 && pos < 6 = 1
    | pos >= 6 && pos < 9 = 2
    | otherwise     = -1 

getMove :: Int -> Int
getMove move
    | move < 3 = move
    | move >= 3 && move < 6 = move - 3
    | move >= 6 && move < 9 = move - 6
    | otherwise     = -1

-- Generic Programming pattern i guess
--¯\_(ツ)_/¯
replace :: Int -> a -> [a] -> [a]
replace ind itm lst = toList $ update ind itm $ fromList lst

changeElem :: Int -> Move -> Board -> Board
changeElem pos move board =
  let row = board !! (getRow pos)
      newRow = replace (getMove pos) move row
   in replace (getRow pos) newRow board

showRow :: Row -> String
showRow row = 
    "| "
    ++ show (head row)
    ++ " | "
    ++ show (row !! 1)
    ++ " | "
    ++ show (row !! 2)
    ++ " |"

showBoard :: Board -> String
showBoard board = 
    "-   -   -   -\t\t-   -   -   -\n"
    ++ showRow (head board) ++ "\t\t| 0 | 1 | 2 |\n"
    ++ "-   -   -   -\t\t-   -   -   -\n"
    ++ showRow (board !! 1) ++ "\t\t| 3 | 4 | 5 |\n"
    ++ "-   -   -   -\t\t-   -   -   -\n"
    ++ showRow (board !! 2) ++ "\t\t| 6 | 7 | 8 |\n"
    ++ "-   -   -   -\t\t-   -   -   -\n"

-- TODO: Move validation so that we don't swap it
{- validateMove :: Game -> Int -> Bool -}
{- validateMove Game{move=mov,board=boa} pos = True -}

makeBoard :: Board
makeBoard = replicate 3 $ replicate 3 Blank

swapPlayer :: Move -> Move
swapPlayer X = O
swapPlayer O = X
swapPlayer Blank = X

checkRow :: Row -> Bool
checkRow [x, y, z]
    | (x == y) && (y == z) && (x /= Blank) = True
    | otherwise   = False

checkWinner :: Board -> Bool
checkWinner [[a, b, c], [d, e, f], [g, h, i]] = 
    or
    [ checkRow [a, b, c],
      checkRow [d, e, f],
      checkRow [g, h, i],
      checkRow [a, d, g],
      checkRow [b, e, h],
      checkRow [c, f, i],
      checkRow [a, e, i],
      checkRow [c, e, g]
    ]

playGame :: Game -> IO()
playGame Game{move=mov,board=boa} = do
    putStr $ showBoard boa
    -- TODO: place exception handlers so that we get only from 1-9 and no strings
    row <- getLine
    let pos = read (head . words $ row) :: Int
    let newBoard = changeElem pos mov boa
    if checkWinner newBoard
      then do
        putStr $ showBoard newBoard
        putStrLn $
          show mov
          ++ " has Won!"
        exitSuccess
    else do
      rPos <- randomRIO (0, 9)
      let nBoard = changeElem rPos (swapPlayer mov) newBoard
      if checkWinner nBoard
        then do
          putStr $ showBoard nBoard
          putStrLn $
            show mov
            ++ " has Won!"
          exitSuccess
      else 
        playGame Game {move=swapPlayer mov,board=nBoard}

main :: IO()
main = do
    putStrLn "\x1b[35mTic Tac Toe\x1b[0m"
    putStrLn "\t\t\t- Written By \x1b[1m\x1b[33mNrdyBhu1\x1b[0m"
    playGame Game {move=X,board=makeBoard}
