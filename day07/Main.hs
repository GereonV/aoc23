import Data.List (elemIndex, sort, sortBy)
import Data.Maybe (fromJust)

data HandType = High | Pair | TwoPair | ThreeOAK | FullHouse | FourOAK | FiveOAK
  deriving (Eq, Ord, Show)

data Hand = Hand { hType :: HandType, cards :: [Int], bid :: Int }
  deriving (Eq, Ord, Show)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

times :: Int -> [a] -> [[a]]
times 0 _ = [[]]
times n l = concatMap (\x -> map (x:) $ times (n - 1) l) l

rateHand :: [Int] -> HandType
rateHand c = case sortBy (flip compare) . map (($ c) . count) $ [0..12] of
    5:_ -> FiveOAK
    4:_ -> FourOAK
    3:2:_ -> FullHouse
    3:_ -> ThreeOAK
    2:2:_ -> TwoPair
    2:_ -> Pair
    _ -> High

hand :: String -> Hand
hand ln = Hand (rateHand cards) cards $ read l
  where
    [f, l] = words ln
    cards = fromJust . flip elemIndex (['2'..'9'] ++ "TJQKA") <$> f

conv :: Hand -> Hand
conv (Hand _ c b) = Hand t nc b
  where
    t = maximum $ map (rateHand . (filter (/= 9) c ++)) (count 9 c `times` [0..12])
    nc = map (\x -> if x == 9 then -1 else x) c

parse :: String -> [Hand]
parse = map hand . lines

solution1 :: [Hand] -> Int
solution1 = sum . zipWith (*) [1..] . map bid . sort

solution2 :: [Hand] -> Int
solution2 = solution1 . map conv

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . parse
