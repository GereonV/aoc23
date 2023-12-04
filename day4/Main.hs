import Data.Bifunctor (first, second)
import Data.Set (intersection, size)
import qualified Data.Set as Set

data Card = Card { winning :: [Int], actual :: [Int] }
  deriving Show

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x (h:t) = if x == h then ([], t) else first (h:) $ splitAtFirst x t

parse :: String -> [Card]
parse = map parseLn . lines
  where
    parseLn ln = Card (map read . fst . splits $ ln) (map read . snd . splits $ ln)
    splits = splitAtFirst "|" . drop 2 . words

matching :: Card -> Int
matching c = size $ intersection (Set.fromList . winning $ c) (Set.fromList . actual $ c)

solution1 :: [Card] -> Int
solution1 = sum . map (score . matching)
  where
    score 0 = 0
    score x = 2 ^ (x - 1)

solution2 :: [Card] -> Int
solution2 = solve . map (, 1)
  where
    solve [] = 0
    solve ((c, a):t) = (a +) . solve $ cops ++ rest
      where
        m = matching c
        cops = second (+ a) <$> take m t
        rest =                  drop m t

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . parse
