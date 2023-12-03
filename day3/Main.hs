import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)

data Position = Position { x :: Int, y :: Int }
  deriving (Eq, Ord, Show)
data Number = Number { num :: Int, pos :: Position }
  deriving Show

splitWhenPair :: (a -> a -> Bool) -> [a] -> [[a]]
splitWhenPair _ [] = []
splitWhenPair _ [x] = [[x]]
splitWhenPair p (h:(h2:t))
    | p h h2 = [h]:rest
    | otherwise = case rest of
        (x:y) -> (h:x):y
        [] -> [[h, h2]]
  where rest = splitWhenPair p (h2:t)

numbersOnLn :: Int -> [(Int, Char)] -> [Number]
numbersOnLn i = map (\c -> Number (read . map snd $ c) $ Position (fst . head $ c) i) . splitWhenPair (\x y -> fst x + 1 /= fst y)

symbOnLn :: Int -> [(Int, Char)] -> Map Position Char
symbOnLn i = Map.fromList . map (\(p, c) -> (Position p i, c))

parse :: String -> ([Number], Map Position Char)
parse = collectTups . lineTup . lines
  where
    symb = filter ((/= '.') . snd) . zip [0..]
    numbers = filter (      isDigit . snd) . symb
    other   = filter (not . isDigit . snd) . symb
    lineTup = zipWith (\i l -> (numbersOnLn i . numbers $ l, symbOnLn i . other $ l)) [0..]
    collectTups l = (concatMap fst l, Map.unions . map snd $ l)

surrounding :: Number -> [Position]
surrounding n = map (uncurry Position) . concat $ vert ++ hor
  where
    Position x y = pos n
    l = length . show . num $ n
    vert = map (flip zip [y - 1, y, y + 1] . repeat) [x - 1, x + l]
    hor  = map (     zip [x - 1, x..x + l] . repeat) [y - 1, y + 1]

solution1 :: [Number] -> Map Position Char -> Int
solution1 l s = sum . map num . filter (any (`Map.member` s) . surrounding) $ l

solution2 :: [Number] -> Map Position Char -> Int
solution2 l s = sum . map (ratio . numbersAround) $ gears
  where
    gears = map fst . filter ((== '*') . snd) . Map.toList $ s
    numField = map (\n -> (num n, surrounding n)) l
    numbersAround p = map fst . filter (elem p . snd) $ numField
    ratio [a, b] = a * b
    ratio _ = 0

main :: IO ()
main = interact $ unlines . map show . zipWith uncurry [solution1, solution2] . repeat . parse
