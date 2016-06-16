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
-- This takes nothing and is a calling example really
secondDemo2 :: (Integer, Integer)
secondDemo2 = second (\ 1 -> 2) (1, 1)

-- when called with `(1, 1)` it returns `(1, Just 1)`
secondDemo3 :: (a, b) -> (a, Maybe b)
secondDemo3 = second Just

-- when called with `(1, Just 1)` it returns `(1, Just 2)`
secondDemo4 :: Num b => (a, Maybe b) -> (a, Maybe b)
secondDemo4 = second $ fmap (+1)

-- The `,` operator creates a tuple from two elements
tupleDemo :: a -> b -> (a, b)
tupleDemo a b = (,) a b
-- can also be expressed as:
-- tupleDemo = (,)
-- which shouldn't really be surprising

-- The `,,` operator creates a triple from three elements
tripleDemo = a -> b -> c -> (a, b, c)
tripleDemo a b c = (,,) a b c
