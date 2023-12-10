parse :: String -> [[Int]]
parse = map (map read . words) . lines

solutionWith :: ([Int] -> Int -> Int) -> [[Int]] -> Int
solutionWith f = sum . map predict
  where
    predict l
        | all (== 0) l = 0
        | otherwise = f l $ predict [y - x | (x, y) <- zip l $ drop 1 l]

main :: IO ()
main = interact $ unlines . map show . zipWith solutionWith [(+) . last, (-) . head] . repeat . parse
