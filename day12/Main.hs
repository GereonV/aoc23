{-# LANGUAGE LambdaCase #-}

import Data.Bifunctor (bimap)
import Data.List (intercalate, tails)
import Data.Map qualified as M

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn y (x : xs)
  | x == y = [] : splitOn y xs
  | otherwise = let (z : zs) = splitOn y xs in (x : z) : zs

parseInput :: String -> [(String, [Int])]
parseInput inp = do
  l <- lines inp
  let [s, g] = words l
  pure (s, read <$> splitOn ',' g)

matchDefects :: String -> Int -> Maybe String
matchDefects = \cases
  [] 0 -> Just []
  [] _ -> Nothing
  ('#' : _) 0 -> Nothing
  (_ : xs) 0 -> Just xs
  ('.' : _) _ -> Nothing
  (_ : xs) n -> matchDefects xs $ n - 1

possibleFixes :: String -> [Int] -> Int
possibleFixes s g = r M.! (s, g)
  where
    r = M.fromList [((st, gt), f st gt) | st <- tails s, gt <- tails g]
    f "" [] = 1
    f "" _ = 0
    f (x : xs) g = case x of
      '?' -> sum $ (`f` g) . (: xs) <$> ".#"
      '.' -> r M.! (xs, g)
      '#' -> case g of
        [] -> 0
        y : ys -> maybe 0 ((r M.!) . (,ys)) . matchDefects xs $ y - 1

part1 :: [(String, [Int])] -> Int
part1 = sum . fmap (uncurry possibleFixes)

part2 :: [(String, [Int])] -> Int
part2 inp = part1 $ bimap (timesFive "?") (timesFive []) <$> inp
  where
    timesFive l = intercalate l . replicate 5

main :: IO ()
main = interact $ unlines . map show . zipWith ($) [part1, part2] . repeat . parseInput
