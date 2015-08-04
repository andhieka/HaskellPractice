import Data.List

-- Question 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Question 8

compress :: (Eq a) => [a] -> [a]
compress = map head . group

compress' :: (Eq a) => [a] -> [a]
compress' (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys

-- Question 9

pack :: (Eq a) => [a] -> [[a]]
pack = group

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' [x] = [[x]]
pack' (x:xs) = if x `elem` (head (pack' xs))
               then (x:(head (pack' xs))):(tail (pack' xs))
               else [x]:(pack' xs)

-- Question 10

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\ g -> ((length g), (head g))) ys
    where ys = pack xs
