import Control.Applicative ((<|>))
import Data.Array (Array, assocs, elems, listArray, (!), (//))
import Data.Char (ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn e = foldr comb [[]]
  where
    comb x l@(y : ys)
      | x == e = [] : l
      | otherwise = (x : y) : ys

hash :: String -> Int
hash = foldl' (\acc x -> (acc + ord x) * 17 `mod` 256) 0

part1 :: [String] -> Int
part1 = sum . map hash

parse :: String -> (String, Maybe Int)
parse s = let (name, action) = splitAt (fromJust $ elemIndex '-' s <|> elemIndex '=' s) s in (name, readMaybe . drop 1 $ action)

focusingPower :: Array Int [(String, Int)] -> Int
focusingPower arr = sum [(boxi + 1) * slot * focalLength | ~(boxi, box) <- assocs arr, ~(slot, (_, focalLength)) <- zip [1 ..] box]

updateBox :: String -> Maybe Int -> [(String, Int)] -> [(String, Int)]
updateBox n Nothing box = filter (\ ~(x, _) -> x /= n) box
updateBox n (Just fl) box = case elemIndex n . map fst $ box of
  Just i -> take i box ++ [(n, fl)] ++ drop (i + 1) box
  Nothing -> box ++ [(n, fl)]

part2 :: [String] -> Int
part2 = focusingPower . foldl' step (listArray (0, 255) $ repeat [])
  where
    step arr s = let (n, a) = parse s in let i = hash n in arr // [(i, updateBox n a $ arr ! i)]

main :: IO ()
main = interact $ unlines . map show . zipWith ($) [part1, part2] . repeat . splitOn ',' . (\[x] -> x) . lines
