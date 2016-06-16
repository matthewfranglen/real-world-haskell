import Control.Arrow

-- This is *not at all clear*

main :: IO ()
main = do
    putStrLn $ show $ secondDemo (+1) (1, 2)

-- The second function takes a function and a tuple of two elements and applies
-- the function to the second element.
-- I have no idea how I was supposed to figure this out without the comments.

-- secondDemo f (a, b) = (a, f b)
secondDemo :: (b -> c) -> (a, b) -> (a, c)
secondDemo f = second f

-- Other examples
secondDemo2 :: (Integer, Integer)
secondDemo2 = second (\ 1 -> 2) (1, 1)

secondDemo3 :: (a, b) -> (a, Maybe b)
secondDemo3 = second Just

secondDemo4 :: Num b => (a, Maybe b) -> (a, Maybe b)
secondDemo4 = second $ fmap (+1)

