data Turn = Turn { red :: Int, green :: Int, blue :: Int}
  deriving Show
data Game = Game { gameId :: Int, turns :: [Turn] }
  deriving Show

splitUsingWhen :: (a -> a) -> (a -> Bool) -> [a] -> [[a]]
splitUsingWhen _ _ [] = []
splitUsingWhen d p (h:t)
    | p h = [d h]:splitUsingWhen d p t
    | otherwise = case splitUsingWhen d p t of
        x:y -> (h:x):y
        _ -> [[h]]

joinPairs :: (a -> a -> b) -> [a] -> [b]
joinPairs _ [] = []
joinPairs j (h:(h2:t)) = j h h2:joinPairs j t

mapWhere :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapWhere m p = map f
  where
    f x
        | p x = m x
        | otherwise = x

endsWith :: Eq a => a -> [a] -> Bool
endsWith c = (== c) . last

turn :: [String] -> Turn
turn = foldr (f . words) (Turn 0 0 0)
  where
    f [ns, col] x
        | col == "red" = x { red =  n }
        | col == "green" = x { green =  n }
        | col == "blue" = x { blue =  n }
      where n = read ns

parseLine :: String -> Game
parseLine ln = Game { gameId = read . init . (!!1) . words $ ln,
                      turns = map turn turnsWithoutDelim }
  where
    turns = joinPairs ((++) . (++ " ")) . drop 2 . words $ ln
    turnsWithoutDelim = splitUsingWhen init (endsWith ';') . mapWhere init (endsWith ',') $ turns

possible :: Turn -> Turn -> Bool
possible pos x = red x <= red pos && green x <= green pos && blue x <= blue pos

needed :: [Turn] -> Turn
needed t = Turn { red   = maximum . map red   $ t,
                  green = maximum . map green $ t,
                  blue  = maximum . map blue  $ t }

solution1 :: [Game] -> Int
solution1 = sum . map gameId . filter (all (possible $ Turn 12 13 14) . turns)

solution2 :: [Game] -> Int
solution2 = sum . map ((\(Turn a b c) -> a * b * c) . needed . turns)

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . map parseLine . lines
