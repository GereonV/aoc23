import Data.Bifunctor (bimap, first, second)
import qualified Data.List as List
import Data.Maybe (fromMaybe, mapMaybe)

-- source, destination, amount
newtype IMap = IMap [(Int, Int, Int)]
  deriving Show

data Input = Input { seeds :: [Int], maps :: [IMap] }
  deriving Show

imap :: [String] -> IMap
imap = IMap . map (tup . map read . words)
  where tup [d, s, n] = (s, d, n)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p (h:t)
    | p h = []:rest
    | null rest = [[h]]
    | otherwise = uncurry (:) . first ((h:) . head) . splitAt 1 $ rest
  where rest = splitWhen p t

mapVal :: IMap -> Int -> Int
mapVal (IMap m) k = maybe k fst $ List.uncons . mapMaybe maybeMap $ m
  where
    maybeMap (s, d, n)
        | s <= k && k < s + n = Just $ k - s + d
        | otherwise = Nothing

-- get output range from start of input range
range :: IMap -> (Int, Int) -> (Int, Int)
range (IMap m) (i, l) = second (min l) . maybe (i, unmapped) (\(s, d, n) -> (d + i - s, n - i + s)) $ List.find (\(s, _, n) -> s <= i && i < s + n) m
  where
    unmapped = case filter (i <) . map (\(s, _, _) -> s) $ m of
        [] -> l
        x -> minimum x - i

ranges :: IMap -> (Int, Int) -> [(Int, Int)]
ranges _ (_, 0) = []
ranges m (i, l) = d:ranges m (i + snd d, l - snd d)
  where d = range m (i, l)

reachable :: IMap -> [(Int, Int)] -> [(Int, Int)]
reachable m = concatMap $ ranges m

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (h:(h2:t)) = (h, h2):pairs t

parse :: String -> Input
parse = uncurry Input . bimap (map read . tail . words . head . head) (map $ imap . drop 1) . splitAt 1 . splitWhen null . lines

solution1 :: Input -> Int
solution1 (Input s m) = minimum . map (($ m) . foldl (flip mapVal)) $ s

solution2 :: Input -> Int
solution2 (Input s m) = minimum . map fst . foldl (flip reachable) (pairs s) $ m

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . parse
