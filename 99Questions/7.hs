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

-- Question 11

data CodeItem a = Single a | Multiple Int a deriving (Show)
codeItem :: (Eq a) => Int -> a -> CodeItem a
codeItem n x
    | n <= 0    = error "CodeItem must have positive number!"
    | n == 1    = Single x
    | otherwise = Multiple n x

encodeModified :: (Eq a) => [a] -> [CodeItem a]
encodeModified xs =
    let ys = encode xs
    in map (\(n, x) -> if n == 1 then Single x else Multiple n x) ys

-- Question 12

decodeModified :: (Eq a) => [CodeItem a] -> [a]
decodeModified ((Single x):xs) = x:(decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)
decodeModified [] = []

-- Question 13

encodeDirectHelper :: (Eq a) => Int -> [a] -> [CodeItem a]
encodeDirectHelper n [x] = [codeItem (n+1) x]
encodeDirectHelper n (x:xs@(y:_))
    | x == y    = encodeDirectHelper (n+1) xs
    | otherwise = [ci] ++ encodeDirectHelper 0 xs
        where ci = codeItem (n+1) x

encodeDirect :: (Eq a) => [a] -> [CodeItem a]
encodeDirect xs = encodeDirectHelper 0 xs


-- Question 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = (replicate 2 x) ++ (dupli xs)

-- Question 15

repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- Question 16

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ 0 = []
dropEvery 
