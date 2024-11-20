import Data.Bifunctor (bimap, first, second, Bifunctor)
import Data.Set (intersection, size)
import qualified Data.Set as Set

data Card = Card { winning :: [Int], actual :: [Int] }
  deriving Show

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x (h:t)
    | h == x = ([], t)
    | otherwise = first (h:) . splitAtFirst x $ t

mapTup :: Bifunctor p => (a -> b) -> p a a -> p b b
mapTup f = bimap f f

parse :: String -> [Card]
parse = map (uncurry Card . mapTup (map read) . splitAtFirst "|" . drop 2 . words) . lines

matching :: Card -> Int
matching c = size . uncurry intersection . mapTup (\f -> Set.fromList . f $ c) $ (winning, actual)

solution1 :: [Card] -> Int
solution1 = sum . map (score . matching)
  where
    score 0 = 0
    score x = 2 ^ (x - 1)

solution2 :: [Card] -> Int
solution2 = solve . map (, 1)
  where
    solve [] = 0
    solve ((c, a):t) = (a +) . solve . uncurry (++) . first (map $ second (+ a)) . splitAt (matching c) $ t

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . parse
