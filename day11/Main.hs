import Data.Function (applyWhen)
import Data.Set qualified as S
import Data.List (scanl')

data PuzzleInput = PuzzleInput {piWidth :: Int, piHeight :: Int, piGalaxies :: [(Int, Int)]}

parseInput :: String -> PuzzleInput
parseInput inp =
  PuzzleInput
    { piWidth = length $ grid !! 0,
      piHeight = length grid,
      piGalaxies = [(x, y) | (y, l) <- zip [1 ..] grid, (x, '#') <- zip [1 ..] l]
    }
  where
    grid = lines inp

expand :: PuzzleInput -> Int -> [(Int, Int)]
expand ~(PuzzleInput w h g) ex = [(x + exX !! x, y + exY !! y) | ~(x, y) <- g]
  where
    expansions sz occ = scanl' (\a x -> applyWhen (x `S.notMember` occ) (+ ex) a) 0 [1 .. sz]
    [exX, exY] = zipWith expansions [w, h] $ S.fromList . (<$> g) <$> [fst, snd]

sumMinDists :: [(Int, Int)] -> Int
sumMinDists [] = 0
sumMinDists (x : xs) = sum (diff x <$> xs) + sumMinDists xs
  where
    diff ~(x1, y1) ~(x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = interact $ unlines . map (show . sumMinDists) . zipWith (flip expand) [1, 999_999] . repeat . parseInput
