import Data.Bifunctor (bimap, Bifunctor)

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

parse :: String -> [(Int, Int)]
parse = uncurry zip . (\[a, b] -> (a, b)) . map (map read . drop 1 . words) . lines

-- time: t
-- time pressing button: p
-- record distance: r
-- d(p) = (t - p) * p = t * p - p^2
-- d'(p) = t - 2 * p = 0 <=> p = t / 2
-- d(p) = r <=> p^2 - t * p + r = 0 <=> p = t / 2 +- sqrt(t^2 / 4 - r)

possibilities :: (Floating a, RealFrac a) => (a, a) -> Int
possibilities (t, r) = (1 +) . uncurry (-) . bimap floor ceiling . both (\f -> f (t / 2) (sqrt $ t^2 / 4 - r)) $ ((+), (-))

solution1 :: [(Int, Int)] -> Int
solution1 = product . map (possibilities . both fromIntegral)

solution2 :: [(Int, Int)] -> Int
solution2 = possibilities . both (read . concatMap show) . unzip

main :: IO ()
main = interact $ unlines . zipWith (show .) [solution1, solution2] . repeat . parse
