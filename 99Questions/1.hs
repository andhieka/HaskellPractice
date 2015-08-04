-- Problem 1
myLast :: [a] -> a
myLast [] = error "Taking last from empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "No second last of empty list"
myButLast [_] = error "No second last of singleton list"
myButLast ([x,_]) = x
myButLast (_:xs) = myButLast xs

myButLast' xs = reverse xs !! 1

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of bounds"
elementAt (_:xs) k
    | k < 1 = error "Index out of bounds"
    | otherwise = elementAt xs (k - 1)

elementAt' :: [a] -> Int -> a
elementAt' xs n = xs !! (n - 1)

-- this third implementation does not cover the case for
-- negative index together with infinite list
elementAt'' :: [a] -> Int -> a
elementAt'' (x:_) 1 = x
elementAt'' (_:xs) i = elementAt'' xs (i-1)
elementAt'' _ _ = error "Index out of bounds"

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (xs == ys)
    where ys = myReverse xs

isPalindrome' xs = xs == (reverse xs)
