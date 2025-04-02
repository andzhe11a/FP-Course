--ex1
say :: Int -> String
say n = 
    if n == 0 then "zero"
    else if n == 1 then "one"
    else if n == 2 then "two"
    else if n == 3 then "three"
    else if n == 4 then "four"
    else if n == 5 then "five"
    else if n == 6 then "six"
    else if n == 7 then "seven"
    else if n == 8 then "eight"
    else if n == 9 then "nine"
    else "Invalid input"

--ex2
commonPrefixLength :: [Int] -> [Int] -> Int
commonPrefixLength l1 l2 = 
    if l1 == [] || l2 == [] then 0  
    else if head l1 == head l2 then 1 + commonPrefixLength (tail l1) (tail l2)  
    else 0  

--ex3
countEvenOddl :: [Int] -> (Int, Int)
countEvenOddl [] = (0, 0) 
countEvenOddl (x:xs) =
    let (evens, odds) = countEvenOddl xs  
    in if even x
        then (evens + 1, odds) 
        else (evens, odds + 1)  

--ex4
pivotl :: Int -> [Int] -> ([Int], [Int])
pivotl _ [] = ([], [])  
pivotl x (y:ys) =
    let (smaller, greaterOrEqual) = pivotl x ys  
    in if y < x
        then (y : smaller, greaterOrEqual)  
        else (smaller, y : greaterOrEqual)  

--ex6
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

--ex7
adjacentElements :: [Int] -> [(Int, Int)]
adjacentElements [] = []  
adjacentElements [_] = [] 
adjacentElements (x:y:xs) =
    if x < y 
        then (x, y) : adjacentElements (y:xs)  
        else adjacentElements (y:xs)  

--ex8
groupsOfX :: [a] -> Int -> [[a]]
groupsOfX [] _ = []
groupsOfX xs n = take n xs : groupsOfX (drop n xs) n

--ex9
flattenl :: [[a]] -> [a]
flattenl [] = []
flattenl (x:xs) = x ++ flattenl xs

--ex10
decode :: [(Int, a)] -> [a]
decode [] = []
decode ((n, x) : xs) = replicate n x ++ decode xs

--ex13/a
removeFirstOcc :: Eq a => a -> [a] -> [a]
removeFirstOcc _ [] = []
removeFirstOcc x (y:ys) =
    if x == y then ys
    else y : removeFirstOcc x ys

--ex13/b
removeAllOcc :: Eq a => a -> [a] -> [a]
removeAllOcc _ [] = []
removeAllOcc x (y:ys) =
    if x == y then removeAllOcc x ys
    else y : removeAllOcc x ys

--ex14
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) =
    if x `elem` xs then removeDuplicates xs
    else x : removeDuplicates xs

--ex16
mergeevenodd :: [Int] -> [Int] -> [Int]
mergeevenodd [] [] = []
mergeevenodd (x:xs) [] = x : mergeevenodd xs []
mergeevenodd [] (y:ys) = y : mergeevenodd [] ys
mergeevenodd (x:xs) (y:ys) = x : y : mergeevenodd xs ys

main :: IO ()
main = do
    print (say 3)
    print (commonPrefixLength [1, 2, 3] [1, 2, 3])
    print (countEvenOddl [1, 2, 3, 4])
    print (pivotl 5 [1, 6, 3, 7, 4, 5])
    print (isPalindrome "anna")
    print (adjacentElements [1, 2, 3, 4])
    print (groupsOfX [1, 2, 3, 4, 5, 6] 2)
    print (flattenl [[1, 2], [3], [4, 5]])
    print (decode [(1, 1), (2, 2), (3, 3)])
    print (removeFirstOcc 2 [1, 2, 2, 3])
    print (removeAllOcc 2 [1, 2, 2, 3])
    print (removeDuplicates [1, 2, 2, 3, 3, 4])
    print (mergeevenodd [1, 2, 3] [4, 5, 6])
