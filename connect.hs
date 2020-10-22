import Data.List
import Control.Monad (mapM)
import Text.Read (readMaybe)

data Board = Board [[Char]]


--Global Data

empty_board :: [String]
empty_board = 
  [
    "......",
    "......",
    "......",
    "......",
    "......",
    "......",
    "......"
  ]

full_board :: [String]
full_board = 
  [
    "XOXO..",
    "OXOOX.",
    "OXOO..",
    "XOXOXO",
    "OOXXO.",
    "XXOXO.",
    "XOOOXO"
  ]

win_in_1_board = 
  [
    "XOXO..",
    "OXOXX.",
    "OOOX..",
    "XOXOXO",
    "OOXXO.",
    "XXOXX.",
    "XOOOX."
  ]

end_in_2_board = 
  [
    "XOXOXO",
    "OXOXX.",
    "OOOXOX",
    "XOXOXO",
    "OOXXOO",
    "XXOXX.",
    "XOOOXO"
  ]

players :: (Char, Char)
players = ('X', 'O')


--Utility functions

other_player :: Char -> Char
other_player player = if (player == fst players) then 'O' else 'X'

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

is_col_full :: [Char] -> Bool
is_col_full col = not (elem '.' col) 

all_the_same :: (Eq a) => [a] -> Bool
all_the_same xs = all (== head xs) (tail xs)

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

max_of_list :: Ord a => [a] -> a
max_of_list = foldr1 (\x y ->if x >= y then x else y)

min_of_list :: Ord a => [a] -> a
min_of_list = foldr1 (\x y ->if x <= y then x else y)



--Game functions

play_in_col player col
  | not (elem '.' col) = error "Column is full"
  | otherwise = fst split_col ++ [player] ++ tail (snd split_col)
                where split_col = splitAt ind col
                      ind = case findIndex (=='.') col of
                         Just n  -> n
                         Nothing -> 0
  
play board player col_i
  | not (elem '.' col) = error "Column is full"
  | otherwise = left_cols ++ [play_in_col player col] ++ right_cols
  where (left_cols, col:right_cols) = splitAt col_i board

piece_at :: [[Char]] -> Int -> Int -> Char
piece_at board col row = (board !! col) !! row 

all_fours :: [[Char]] -> [[Char]]
all_fours board = concat $ concat [[north_east_fours board col row | col <- [0..6]] | row <- [0..5]]

north_east_fours :: [[Char]] -> Int -> Int -> [[Char]]
north_east_fours board col row
  | col < 4 && row < 3 = [north, northeast, east]
  | col > 3 && row < 3 = [north]
  | col < 4 && row > 2 = [east]
  | col > 3 && row > 2 = []
  where
    piece_at_coords (col, row) = piece_at board col row
    add_initial_coords (col, row) = map (\(c, r) -> (c + col, r + row))
    pieces_with_offsets = map piece_at_coords . (add_initial_coords (col, row))
    north =     pieces_with_offsets [(0, 0), (0, 1), (0, 2), (0, 3)]
    northeast = pieces_with_offsets [(0, 0), (1, 1), (2, 2), (3, 3)]
    east =      pieces_with_offsets [(0, 0), (1, 0), (2, 0), (3, 0)]

legal_moves board = [col_n | col_n <- [0..6], not $ is_col_full $ board !! col_n]

winner :: [[Char]] -> Char
winner board
  | length winning_fours > 0 = head $ head winning_fours
  | legal_moves board == [] = 'N'
  | otherwise = '-' 
  where winning_fours =filter (/= "....") $ filter all_the_same $ all_fours board
  




--AI

possible_next_boards :: [[Char]] -> Char -> [[[Char]]]
possible_next_boards board player =
  map (play board player) (legal_moves board)

--strength: 0: loosing, 1: unknown, 2:winning
strength_of_move :: [[Char]] -> Char -> Int -> Int
strength_of_move board player move
  | winning_player == player = 2
  | winning_player == (other_player player) = 0
  | otherwise = 1
  where winning_player = (winner (play board player move))


value_of_board :: [[Char]] -> Char -> Float
--here player is the one that just played
value_of_board board player
  | winner board == 'X' = 1.0
  | winner board == 'N' = 0.5
  | winner board == 'O' = 0.0
  | otherwise = case player of
    'X' -> max_of_list outcomes
    'O' -> min_of_list outcomes 
    where outcomes = map (\b -> value_of_board b (other_player player)) (possible_next_boards board (other_player player))
  

find_best_move :: [[Char]] -> Char -> Int
find_best_move board player = undefined
  {-case player of
    'X' -> foldr1 (compare_moves (>=)) (legal_moves board)
    'O' -> foldr1 (compare_moves (<=)) (legal_moves board)
    where
      value_of_move = value_of_board . (play board player)
      compare_moves comp = (\x y ->if (comp (value_of_move x) (value_of_move y)) then x else y)-}


--IO

render :: [[Char]] -> IO ()
render board =
  putStrLn . concat . reverse $
    map (\i ->
      "|" ++ (intersperse '|' (map (\col -> case (col !! i) of 
        '.' -> ' '
        x -> x
      ) board)) ++ "|\n"
    ) [0..5]


query :: Read a => String -> IO a
query prompt = do
  putStr $ prompt ++ ": "
  val <- readMaybe <$> getLine
  case val of
    Nothing -> do
      putStrLn "Please enter a number"
      query prompt
    Just v -> return v


prompt_for_move :: Char -> [[Char]] -> IO [[Char]]
prompt_for_move player board = do
  render board
  col <- query $ "Player " ++ show player ++ ", please enter the number of the column where you want to play"
  return $ play board player (col - 1)
  

game :: Char -> [[Char]] -> IO ([[Char]], Char)
game player board = do
  new_board <- prompt_for_move player board
  case (winner new_board) of
    'X' -> return (new_board, 'X')
    'O' -> return (new_board, 'O')
    _   -> game (other_player player) new_board


main :: IO ()
main = do
  putStrLn "Welcome to the game of Connect four - 2 players mode\n"
  (board, winner) <- game 'X' empty_board
  render board  
  putStrLn $ "Player '" ++ [winner] ++ "' has won the game, congratulations!"









{-
  Board:
  | | | | | | | |
  | | | | | | | |
  | | | | | | | |
  | | | |O| | | |
  | | | |X| |X| |
  |O|X|O|X|O|X|O|
-}







