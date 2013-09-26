
import System.Random
import Data.List
import Data.Tuple
import Data.Ord
import Data.Maybe

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

-- Question 11
data Encoding a = Single a | Multiple Int a
    deriving (Show)
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map conv . encode
    where conv (1,x) = Single x
          conv (n,x) = Multiple n x

-- Question 12
decodeModified :: [Encoding a] -> [a]
decodeModified = concat . map conv
    where conv (Single x) = [x]
          conv (Multiple n x) = replicate n x

-- Question 13
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) =
    let (y:ys) = encodeDirect xs in
    if x == elem y
    then ((inc y):ys)
    else [Single x] ++ (y:ys)
    where
        elem (Single x) = x
        elem (Multiple _ x) = x
        inc (Single x) = Multiple 2 x
        inc (Multiple n x) = Multiple (n+1) x

-- Question 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)
-- magic: concatMap

-- Question 15
repli :: [a] -> Int -> [a]
repli xs n = concat $ map (replicate n) xs

-- Question 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [ x | (k,x) <- zip [1..] xs, k `mod` n /= 0 ]

-- Question 17
split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

-- Question 18
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k-i+1) $ drop (i-1) xs

-- Question 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n
    | n > 0 = (drop n xs) ++ (take n xs)
    | n < 0 = reverse $ rotate (reverse xs) (-n)

-- Question 20
removeAt :: [a] -> Int -> (a,[a])
removeAt [] _ = error "index out of range"
removeAt (x:xs) k
    | k == 1 = (x,xs)
    | k > 1  = let (y,ys) = removeAt xs (k-1)
               in (y,x:ys)

-- Question 21
insertAt :: a -> [a] -> Int -> [a]
insertAt z [] k
    | k == 1    = [z]
    | otherwise = error "index out of range"
insertAt z (x:xs) k
    | k == 1 = z:x:xs
    | k > 1  = x : insertAt z xs (k-1)

-- Question 22
range i j = [i..j]

-- Question 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect _ 0 = return []
rndSelect xs n = do
    k <- randomRIO (1,length xs)
    ys <- rndSelect (snd $ removeAt xs k) (n-1)
    return $ (xs !! (k-1)) : ys

-- Question 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m =
    rndSelect [1..m] n

-- Question 25
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect xs $ length xs

-- Question 26
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (:[]) xs
combinations n (x:xs) = [ x:c | c <- combinations (n-1) xs ]
                        ++ combinations n xs

-- Question 27
-- skipped. couldn't find clean single-function solution

-- Question 28
lsort = sortBy (comparing length)

-- Question 31
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (/= 0) $ map (n `mod`) $ 2:[3,5..k]
    where k = floor.sqrt $ fromIntegral n

-- Question 32
myGcd :: Integral a => a -> a -> a
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)

-- Question 33
coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1

-- Question 34
totient :: Integral a => a -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..n-1]

-- Question 35
factor :: Integral a => a -> [a]
factor n = helper n 2
    where helper n p
            | n == 1         = []
            | n `mod` p == 0 = p : helper (n `div` p) p
            | otherwise      = helper n (p+1)

-- Question 36
factorMult :: Integral a => a -> [(a,Int)]
factorMult n = map swap $ encode $ factor n

-- Question 37
totient' :: Integral a => a -> a
totient' n = product [ (p-1) * p^(m-1) | (p,m) <- factorMult n ]

-- Question 39
primesR a = filter isPrime . range a

-- Question 40
goldbach :: Integral a => a -> (a,a)
goldbach n = head $ [ (x,y) | x <- ps, y <- ps, x+y == n ]
    where ps = primesR 2 (n-1)

-- Question 41
goldbachList n m = map goldbach $ filter even [n..m]


-- utils for chapter 4
bshow :: [Bool] -> String
bshow ps = concatMap str ps
    where str True  = "True  "
          str False = "False "

btblf :: Int -> [[Bool]]
btblf 1 = [[True], [False]]
btblf n = let tbl = btblf (n-1)
         in (map (True:) tbl) ++ (map (False:) tbl)

-- Question 46
-- skipped predicates
table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
    mapM_ (putStrLn.bshow) [ [p,q,f p q] | [p] <- btblf 1, [q] <- btblf 1 ]

-- Question 48
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = do mapM_ (putStrLn.bshow) [ ps ++ [f ps] | ps <- btblf n]

-- Question 49
gray 0 = [""]
gray n = [ '0':c | c <- cs ] ++ reverse [ '1':c | c <- cs ]
    where cs = gray (n-1)

-- Question 50
-- NOTE: not efficient at all, but easy to read & straght-forward
huffman :: (Ord a, Ord f, Num f) => [(a,f)] -> [(a,[Char])]
huffman xs =
    let fold [t] = [t]
        fold ((f1,cs1):(f2,cs2):ts)
            = fold $ insert (f1+f2, [ (w,'0':c) | (w,c) <- cs1 ] ++
                                    [ (w,'1':c) | (w,c) <- cs2 ]    ) ts
    in sortBy (comparing fst) $ snd.head $
        fold [ (f,[(w,[])]) | (w,f) <- sortBy (comparing snd) xs ]

-- Tree definition
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- Question 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [ Branch 'x' l r | m <- [h..h+r],
                                l <- cbalTree m,
                                r <- cbalTree (n-m-1) ]
    where h = (n-1) `quot` 2
          r = (n-1) `mod` 2

-- Question 56
-- as suggested in the question...
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2)
    = mirror l1 r2 && mirror r1 l2
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- Question 57
-- add...? wth is that?
construct :: Ord a => [a] -> Tree a
construct xs = foldl add Empty xs
    where add Empty x = Branch x Empty Empty
          add (Branch v l r) x =
            if v < x then Branch v l (add r x)
                     else Branch v (add l x) r

-- Question 58
symCbalTree :: Int -> [Tree Char]
symCbalTree = (filter symmetric) . cbalTree

-- Question 59
-- Question 60
-- skipped. Ambiguous & looks tedious

-- Question 61
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- Question 61A
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ l r) = leaves l ++ leaves r

-- Question 62
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v l r) = v : internals l ++ internals r

-- Question 62A
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch v l r) lv
    | lv == 1   = [v]
    | otherwise = atLevel l (lv-1) ++ atLevel r (lv-1)

-- Question 63
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = add 1
    where add i
            | i <= n    = Branch 'x' (add (i*2)) (add (i*2+1))
            | otherwise = Empty

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = test t 1 (count t)
    where count Empty          = 0
          count (Branch _ l r) = count l + count r + 1
          test Empty          i n = (i > n)
          test (Branch _ l r) i n = test l (i*2) n && test r (i*2+1) n
          
-- Question 64
layout :: Tree a -> Tree (a,(Int,Int))
layout t = fst $ walk 1 1 t
    where walk x y Empty = (Empty, x)
          walk x y (Branch v l r) =
            let (l',x' ) = walk x      (y+1) l
                (r',x'') = walk (x'+1) (y+1) r
            in (Branch (v,(x',y)) l' r', x'')


layout' :: Tree a -> Tree (a,(Int,Int))
layout' t = fst $ walk 1 1 t
    where walk x y Empty = (Empty, x)
          walk x y (Branch v l r) =
            let (l',x' ) = walk x      (y+1) l
                (r',x'') = walk (x'+1) (y+1) r
            in (Branch (v,(x',y)) l' r', x'')

-- Question 65
-- Question 66
-- Question 67
-- Question 68
-- Question 69
-- skipped. Just tedious.


-- Question 90
queen :: Int -> [[Int]]
queen n = map reverse $ add n
    where add 1 = map (\x->[x]) [1..n]
          add i = [ x:s | s <- add (i-1), x <- [1..n],
                          x `notElem` s, noDiag x s ]
          noDiag x s = all (\(dy,z) -> abs(z-x) /= dy) $ zip [1..] s

