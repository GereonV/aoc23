import Data.Bifunctor (bimap, first, second)
import qualified Data.List as List
import Data.Maybe (fromMaybe, listToMaybe)

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
    | otherwise = uncurry (:) . first ((h:) . fromMaybe [] . listToMaybe) . splitAt 1 $ rest
  where rest = splitWhen p t

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (h:(h2:t)) = (h, h2):pairs t

mapVal :: IMap -> Int -> Int
mapVal (IMap m) k = fromMaybe k $ listToMaybe [k - s + d | (s, d, n) <- m, s <= k && k < s + n]

mapStart :: IMap -> (Int, Int) -> (Int, Int)
mapStart (IMap m) (i, l) = second (min l) . fromMaybe (i, unmapped) $ listToMaybe [(d + i - s, n - i + s) | (s, d, n) <- m, s <= i && i < s + n]
  where
    unmapped = case filter (i <) . map (\(s, _, _) -> s) $ m of
        [] -> l
        x -> minimum x - i

ranges :: IMap -> (Int, Int) -> [(Int, Int)]
ranges _ (_, 0) = []
ranges m (i, l) = x:ranges m (i + snd x, l - snd x)
  where x = mapStart m (i, l)

parse :: String -> Input
parse = uncurry Input . bimap (map read . tail . words . head . head) (map $ imap . drop 1) . splitAt 1 . splitWhen null . lines

solution1 :: Input -> Int
solution1 (Input s m) = minimum . foldl (flip $ map . mapVal) s $ m

solution2 :: Input -> Int
solution2 (Input s m) = minimum . map fst . foldl (flip $ concatMap . ranges) (pairs s) $ m

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . parse
