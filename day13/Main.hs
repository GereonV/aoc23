import Data.List (transpose)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn e (x : xs)
  | e == x = [] : splitOn e xs
  | otherwise = let y : ys = splitOn e xs in (x : y) : ys

findHorizontalReflections :: [String] -> [Int]
findHorizontalReflections pattern = filter isHorizontalReflection possibleHorizontals
  where
    possibleHorizontals = [x | (x, a, b) <- zip3 [1 ..] pattern $ drop 1 pattern, a == b]
    isHorizontalReflection i = all (uncurry (==)) $ zip (reverse $ take i pattern) (drop i pattern)

findReflections :: [String] -> [Int]
findReflections pattern = do
  ~(fac, pat) <- [(100, pattern), (1, transpose pattern)]
  ref <- findHorizontalReflections pat
  pure $ fac * ref

smudges :: [String] -> [[String]]
smudges [] = []
smudges (x : xs) = ((: xs) <$> xSmudges) ++ ((x :) <$> smudges xs)
  where
    xSmudges = [take (n - 1) x ++ [if x !! (n - 1) == '.' then '#' else '.'] ++ drop n x | n <- [1 .. length x]]

part1 :: [[String]] -> Int
part1 = sum . map (\[r] -> r) . map findReflections

part2 :: [[String]] -> Int
part2 patterns = (`div` 2) . sum $ do
  pat <- patterns
  og <- findReflections pat -- only value as by part 1
  smudged <- smudges pat
  x <- findReflections smudged -- any amount of `og`s and new value exactly twice
  if x == og then [] else pure x

main :: IO ()
main = interact $ unlines . map show . zipWith ($) [part1, part2] . repeat . splitOn "" . lines
