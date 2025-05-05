{-
25.2. Да се дефинира функция, която по два списъка намира дължината на
най-дългия им общ префикс.

25.3. Да се дефинира функция countEvenOddl, която за списъка от цели числа
l връща наредена двойка от броя на четните и броя на нечетните числа
в l.

25.4. Да се дефинира функция pivotl x, която за списъца от числа l и числото
x връща наредена двойка (l1, l2), където l1 е списък от елементите на l,
по-малки от x, а l2 е списък от елементите на l, по-големи или равни на x.

25.5. Да се дефинира функция, която в даден низ замества всички малки
латински букви със съответните им големи латински букви.

25.6. Да се дефинира функция, която проверява дали даден низ е палиндром,
т.е. дали се е еднакъв при четене от ляво на дясно и от дясно на ляво.

25.7. Да се дефинира функция, която по даден списък от цели числа l връща
списък от всички двойки (a, b) от l, за които a и b са съседни елементи
в l и a < b.

25.8. Да се дефинира функция groupsof l x, която разделя списъка l на гру-
пи от по x елемента.
Например, groupsof [1, 2, 3, 4, 5, 6, 7, 8] 3 → [[1, 2, 3], [4, 5, 6], [7, 8]].

25.9. Да се дефинира функция flattenl :: [[a]] → [a], която получава списък от
списъци и връща списък, който съдържа всички елементи на входните
списъци.

25.10. Да се дефинира функция decode l :: [(Int, a)] → [a], която получава спи-
сък от двойки и връща списък, който съдържа всички елементи на вход-
ния списък, като всеки елемент се повтаря толкова пъти, колкото е ука-
зано в първия елемент на двойката.

25.11. Да се дефинира функция pack l :: [a] → [[a]], която получава списък и
връща списък от списъци, като всеки от тях съдържа всички последо-
вателни еднакви елементи на входния списък.
Например, pack [1, 1, 1, 2, 2, 3, 4, 4, 4, 4] → [[1, 1, 1], [2, 2], [3], [4, 4, 4, 4]]

25.12. Да се дефинира функция encode l :: [a] → [(Int, a)], която получава спи-
сък и връща списък от двойки, където първият елемент на двойката е
броят на последователните еднакви елементи от входния списък, а вто-
рият елемент е самият елемент.
Например, encode [1, 1, 1, 2, 2, 3, 4, 4, 4, 4] → [(3, 1),(2, 2),(1, 3),(4, 4)].

25.13. Да се дефинира функция remove x l, която премахва (а) първото сре-
щане на елемента x от списъка l и (б) всички срещания на елемента x
от списъка l.

25.14. Да се дефинира функция removeDuplicates l, която премахва всички
повторения на елементите на списъка l.

25.15. За даден списък L, да се намерят елементите на списъка, чиято стой-
ност е по-голяма от сумата на предхождащите ги елементи. Пример
[1, 2, 5, 9, 16] → [1, 2, 5, 9].

25.16. Да се дефинира функция mergeevenodd l1 l2, която получава два спи-
съка от цели числа l1 и l2 и връща списък, чиито елементи на четни
позиции са елементите на l1, а тези на нечетни позиции са елементите
на l2. Пример: mergeevenodd [1,2,3] [4,5,6] -> [1,4,2,5,3,6].
-}

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
