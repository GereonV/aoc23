import qualified Data.Map.Strict as Map
import Data.List (foldl', scanl')
import Data.Map.Strict (Map, (!))

data Node = Node { name :: String, left :: String, right :: String }
  deriving Show

data Input = Input { instructions :: String, nodes :: Map String Node }
  deriving Show

parse :: String -> Input
parse s = Input i $ Map.fromList . map ((\x -> (name x, x)) . inp) $ n
  where
    i:_:n = lines s
    inp = (\[n, _, l, r] -> Node n (tail . init $ l) (init r)) . words

next :: Map String Node -> Node -> Char -> Node
next m n d = m ! (if d == 'L' then left else right) n

steps :: Input -> (Node -> Bool) -> Node -> Int
steps (Input i n) f x = length . takeWhile f . scanl' (next n) x $ cycle i

solution1 :: Input -> Int
solution1 inp = steps inp ((/= "ZZZ") . name) $ nodes inp ! "AAA"

-- assumptions:
-- - every xxA is in a cycle with exactly one xxZ
-- - steps between xxA and xxZ = steps between xxZ and xxZ
-- how do I know this? I looked up solutions and that's what everyone did
solution2 :: Input -> Int
solution2 inp = foldl' lcm 1 $ map (steps inp ((/= 'Z') . last . name)) starts
  where starts = filter ((== 'A') . last . name) . map snd . Map.toList . nodes $ inp

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . parse
