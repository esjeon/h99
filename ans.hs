
-- Question 1
myLast :: [a] -> a 
myLast [] = error "list too short"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Question 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Question 3
elemAt :: [a] -> Int -> a
elemAt [] _ = error "index out of range"
elemAt (x:xs) i
    | i == 1    = x
    | i > 1     = elemAt xs (i - 1)
    | otherwise = error "index out of range"

-- Question 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

-- Question 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Question 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == (reverse xs)

-- Question 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = foldl (\ys xs -> ys ++ (flatten xs)) [] x
-- magic: concatMap

-- Question 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x1:(x2:xs))
    | x1 == x2  = compress (x1:xs)
    | otherwise = (x1:compress (x2:xs))

-- Question 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) =
    let ((y:ys):yss) = pack xs in
    if x == y
    then (([x,y] ++ ys):yss)
    else [[x]] ++ ((y:ys):yss)
-- magic: span in Data.List

-- Question 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1,x)]
encode (x:xs) =
    let ((n,y):ys) = encode xs in
    if x == y
    then ((n+1,y):ys)
    else [(1,x)] ++ ((n,y):ys)
-- magic: group in Data.List

