import Data.Char (digitToInt)

myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength []     = 0

fLength :: [a] -> Int
fLength xs = foldr f 0 xs
    where f v a = a + 1

myNull :: [a] -> Bool
myNull [] = True
myNull _  = False

fNull :: [a] -> Bool
fNull xs = foldr f True xs
    where f _ _ = False

myHead :: [a] -> a
myHead (x:xs) = x
myHead []     = error "Empty list"

fHead :: [a] -> Maybe a
fHead xs = foldr f Nothing xs
    where f v _ = Just v

myTail :: [a] -> [a]
myTail (x:xs) = xs
myTail []     = error "Empty list"

fTail :: [a] -> Maybe [a]
fTail xs = xs'
    where (xs', _)      = foldr f (Nothing, []) xs
          f x (_, xs'') = (Just xs'', x:xs'')

myLast :: [a] -> a
myLast (x:[])   = x
myLast (x:y:xs) = myLast (y:xs)
myLast []       = error "Empty list"

-- This uses a tuple with a function at the end which initially wraps the value
-- in Just, but then becomes a function which returns the result of the last
-- function (which is that first encountered value in a Just)
fLast :: [a] -> Maybe a
fLast xs = x
    where (x, _) = foldr f (Nothing, Just) xs
          f x (_, g) = (x', \_ -> x')
            where x' = g x

myInit :: [a] -> [a]
myInit (x:[])   = []
myInit (x:y:xs) = (x : myInit (y:xs))
myInit []       = error "Empty list"

-- Can't seem to find a way that isn't conditional
fInit :: [a] -> Maybe [a]
fInit xs = xs'
    where (xs', _)    = foldr f (Nothing, []) xs
          f _ (Nothing, _) = (Just [], [])
          f x (_, xs)      = (Just xs', xs')
            where xs'      = x:xs


makeSafe f [] = Nothing
makeSafe f xs = Just $ f xs

-- safeHead xs = makeSafe myHead xs

myAppend :: [a] -> [a] -> [a]
myAppend (x:xs) ys = (x : myAppend xs ys)
myAppend [] (y:ys) = (y : myAppend [] ys)
myAppend [] []     = []

myConcat :: [[a]] -> [a]
myConcat ((x:xs):xss) = (x : myConcat (xs:xss))
myConcat ([]:xss)     = myConcat xss
myConcat []           = []

myReverse :: [a] -> [a]
myReverse (x:xs) = (myReverse xs) ++ [x]
myReverse []     = []

myAnd :: [Bool] -> Bool
myAnd (False:xs) = False
myAnd (True:[])  = True
myAnd (True:xs)  = myAnd xs
myAnd []         = error "Empty list"

myOr :: [Bool] -> Bool
myOr (True:xs)  = True
myOr (False:[]) = False
myOr (False:xs) = myOr xs
myOr []         = error "Empty list"

myAll :: (a -> Bool) -> [a] -> Bool
myAll f (x:[]) = f x
myAll f (x:xs) = if f x
                 then False
                 else myAll f xs
myAll f []     = error "Empty list"

myAny :: (a -> Bool) -> [a] -> Bool
myAny f (x:[]) = f x
myAny f (x:xs) = if f x
                 then True
                 else myAny f xs
myAny f []     = error "Empty list"

myTake :: Int -> [a] -> [a]
myTake 0 _      = []
myTake n (x:xs) = (x : myTake (n - 1) xs)
myTake _ []     = []

myDrop :: Int -> [a] -> [a]
myDrop 0 xs     = xs
myDrop n (x:xs) = myDrop (n - 1) xs
myDrop _ []     = []

mySplit :: Int -> [a] -> ([a], [a])
mySplit n (x:xs) = do
    let (ys, zs) = mySplit (n - 1) xs
    if n > 0
    then ((x:ys), zs)
    else (ys, (x:zs))
mySplit _ []     = ([], [])

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) = if f x
                       then x:myTakeWhile f xs
                       else []
myTakeWhile f [] = []

-- naive approach - the list is traversed from the end so this is the wrong side
-- fTakeWhile :: (a -> Bool) -> [a] -> [a]
-- fTakeWhile f xs = f'
--     where (f', _)         = foldr g ([], True) xs
--           g x (xs', True) = if f x then (x:xs', True) else (xs', False)
--           g _ a           = a

fTakeWhile :: (a -> Bool) -> [a] -> [a]
fTakeWhile f xs = foldr f' [] xs
    where f' x xs = if f x
                    then x:xs
                    else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (x:xs) = if f x
                       then myDropWhile f xs
                       else x:xs
myDropWhile f [] = []

myBreak :: (a -> Bool) -> [a] -> ([a], [a])
myBreak f (x:xs) = if f x
                   then ([],(x:xs))
                   else ((x:ys), zs)
    where (ys, zs) = myBreak f xs
myBreak f [] = ([], [])

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f xs = myBreak (\a -> not $ f a) xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem a (x:xs) = if a == x
                  then True
                  else myElem a xs
myElem _ [] = False

myNotElem :: (Eq a) => a -> [a] -> Bool
myNotElem a xs = not $ myElem a xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f (x:xs) = if f x
                    then (x : myFilter f xs)
                    else myFilter f xs
myFilter f [] = []

myIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
myIsPrefixOf (x:xs) (y:ys)
    | x == y      = myIsPrefixOf xs ys
myIsPrefixOf [] _ = True
myIsPrefixOf _ _  = False

myIsInfixOf :: (Eq a) => [a] -> [a] -> Bool
myIsInfixOf [] _ = True
myIsInfixOf xs (y:ys)
    | myIsPrefixOf xs (y:ys) = True
    | myIsInfixOf xs ys      = True
myIsInfixOf _ _ = False

myIsSuffixOf :: (Eq a) => [a] -> [a] -> Bool
myIsSuffixOf [] [] = True
myIsSuffixOf xs (y:ys)
    | xs == (y:ys)       = True
    | myIsSuffixOf xs ys = True
myIsSuffixOf _ _ = False

myZip :: [a] -> [a] -> [(a,a)]
myZip [] [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
myZip _ _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys
myZipWith _ _ _ = []

myLines :: [Char] -> [[Char]]
myLines [] = []
myLines xs = case break isNewLine xs of
    ([],(y:ys)) -> myLines ys
    (ys,(z:zs)) -> ys : myLines zs
    (ys,[])     -> [ys]
isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine '\r' = True
isNewLine _    = False

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ myInit xs

-- removing version
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = case break f xs of
    ([],(y:ys)) -> splitWith f ys
    (ys,(z:zs)) -> ys : splitWith f zs
    (ys,[])     -> [ys]


-- asInt :: [Char] -> Int
-- asInt = foldl step 0
--     where step a x | x == '-'  = -a -- at this point a is zero so this doesn't work
--                    | otherwise = (a * 10) + digitToInt x

asInt :: [Char] -> Int
asInt xs = let (_, a) = foldr step (1, 0) xs
    in a
    where step value (factor, acc) | value == '-' = (factor, -acc)
                                   | otherwise    = (factor * 10, acc + (factor * digitToInt value))

fConcat :: [[a]] -> [a]
fConcat = foldr (++) []

-- groupBy splits into equal adjacent elements
-- currently not quite perfect:
--    let g a _ = a == 'e'
--    groupBy g "hello"
-- this produces ["h", "ello"] when fGroupBy produces ["h","el","l","o"]
-- I'm not super sure what groupBy is supposed to do so :shrug:
fGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
fGroupBy f xs = xs':xss
    where (xss, xs') = foldr f' ([], []) xs
          f' x (yss, []) = (yss, [x])
          f' x (yss, ys@(y:_)) = if f x y
                             then (yss, x:ys)
                             else (ys:yss, [x])

fAny :: (a -> Bool) -> [a] -> Bool
fAny f xs = foldr f' False xs
    where f' x True = True
          f' x _    = f x

-- This cycles the list by operating over the generated list
fCycle :: [a] -> [a]
fCycle xs = foldr f [] [1..]
    where f _ xs' = xs ++ xs'

fWords :: [Char] -> [[Char]]
fWords xs = if null xs' then xss else xs':xss
    where (xss, xs') = foldr f ([], []) xs
          f x (yss, []) = if isNewLine x then (yss, []) else (yss, [x])
          f x (yss, ys) = if isNewLine x then (ys:yss, []) else (yss, x:ys)
