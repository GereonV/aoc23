import Data.Function (applyWhen)
import Data.Set qualified as S

data PuzzleInput = PuzzleInput {piWidth :: Int, piHeight :: Int, piGalaxies :: [(Int, Int)]}

parseInput :: String -> PuzzleInput
parseInput inp = PuzzleInput width height galaxies
  where
    grid = lines inp
    height = length grid
    width = length $ grid !! 0
    galaxies = [(x, y) | (y, l) <- zip [1 ..] grid, (x, '#') <- zip [1 ..] l]

expand :: PuzzleInput -> Int -> [(Int, Int)]
expand PuzzleInput {piWidth = w, piHeight = h, piGalaxies = g} ex = [(x + exX !! x, y + exY !! y) | (x, y) <- g]
  where
    [occupiedCols, occupiedRows] = S.fromList . (<$> g) <$> [fst, snd]
    expansions sz occ = scanl (\a x -> applyWhen (x `S.notMember` occ) (+ ex) a) 0 [1 .. sz]
    [exX, exY] = uncurry expansions <$> [(w, occupiedCols), (h, occupiedRows)]

sumMinDists :: [(Int, Int)] -> Int
sumMinDists [] = 0
sumMinDists (x : xs) = sum (diff x <$> xs) + sumMinDists xs
  where
    diff (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = interact $ unlines . map (show . sumMinDists) . zipWith (flip expand) [1, 999999] . repeat . parseInput
