{-
28.1. Направете и тествайте собствена реализация на функциите map, filter
и fold(l,r,l1,r1).

28.2. Нека е даден списък l::[(Int,Int,Int)] с тройки 
(ai, bi, ci). С помощта на map, fold и filter да се намерят:
а) Списъка от сумите на елементите на тройките [(ai + bi + ci)]
б) Тройка от сумите на отделните компоненти на елемнтите на l,
(Pai, Pbi, Pci)
в) Броя на тройките, за които ai + bi > ci
г) Дали има поне една тройка, за която ai = bi = ci (True или False)

28.3. За списък от числа L да се намери списък с само с тези числа, които
съвпадат с поредния си номер в L. Например [1, 5, 3, 4, 2] → [1, 3, 4].

28.4. За списък от числа L да се намери списък със сумите на всички двойки
последователни елементи на L. Например [1, 5, 3, 4, 2] → [6, 8, 7, 6].
-}

--ex1
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc []     = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ []     = error "empty list"
myFoldl1 f (x:xs) = foldl f x xs

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ []     = error "empty list"
myFoldr1 f [x]    = x
myFoldr1 f (x:xs) = f x (myFoldr1 f xs)

--ex2
type Triple = (Int, Int, Int)

triples :: [Triple]
triples = [(1, 2, 3), (4, 5, 6), (7, 3, 9), (2, 2, 2)]

sumOfTriples :: [Triple] -> [Int]
sumOfTriples = myMap (\(a, b, c) -> a + b + c)

sumComponents :: [Triple] -> Triple
sumComponents = myFoldr (\(a, b, c) (sa, sb, sc) -> (a + sa, b + sb, c + sc)) (0, 0, 0)

countABGreaterThanC :: [Triple] -> Int
countABGreaterThanC = length . myFilter (\(a, b, c) -> a + b > c)

hasEqualTriple :: [Triple] -> Bool
hasEqualTriple = any (\(a, b, c) -> a == b && b == c)

--ex3
matchingIndices :: [Int] -> [Int]
matchingIndices xs = [x | (i, x) <- zip [0..] xs, i == x]

--ex4
sumPairs :: [Int] -> [Int]
sumPairs xs = [a + b | (a, b) <- zip xs (tail xs)]

main :: IO ()
main = do
    --ex1
    print $ myMap (*2) [1,2,3]         
    print $ myFilter even [1..10]       
    print $ myFoldl (+) 0 [1,2,3,4]     
    print $ myFoldr (:) [] [1,2,3]      
    print $ myFoldl1 (-) [10,1,2]      
    print $ myFoldr1 (-) [10,1,2]       

    --ex2
    print $ sumOfTriples triples        
    print $ sumComponents triples        
    print $ countABGreaterThanC triples  
    print $ hasEqualTriple triples       

    --ex3
    print $ matchingIndices [1, 5, 3, 4, 2]  

    --ex4
    print $ sumPairs [1, 5, 3, 4, 2]