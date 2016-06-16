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
