--дали цифрите на трицифреното число m са различни
allDiigitsAreDiff :: Int -> Bool
allDiigitsAreDiff m =
    let a = m `div` 100
        b = (m `div` 10) `mod` 10
        c = m `mod` 10
    in m > 99 && m < 1000 &&
    a /= b && b /= c && a /= c

--дали цялото число p се дели на 4 или на 7
numIsDivisableBy4Or7 :: Int -> Bool
numIsDivisableBy4Or7 p = 
    p `mod` 4 == 0 ||
    p `mod` 7 == 0

--дале десетичните записи на трицифрените естествени числа x и y са симетрични
areSymmetric :: Int -> Int -> Bool
areSymmetric x y =
    let x1 = x `div` 100
        x2 = (x `div` 10) `mod` 10
        x3 = x `mod` 10

        y1 = y `div` 100
        y2 = (y `div` 10) `mod` 10
        y3 = y `mod` 10

    in x > 99 && x < 1000 && y > 99 && y < 1000 &&
        x1 == y3 && x2 == y2 && x3 == y1

--дали естественото число x, за което се знае, че е по-малко от 23, е просто
isPrimeAndLessThan23 :: Int -> Bool
isPrimeAndLessThan23 x = x < 23 && isPrime x

isPrime :: Int -> Bool
isPrime x = isPrimeHelper x 2

isPrimeHelper :: Int -> Int -> Bool
isPrimeHelper x d
    | x < 2     = False   
    | d * d > x = True    
    | mod x d == 0 = False 
    | otherwise  = isPrimeHelper x (d + 1)

--колко пъти се среща число k 
countKOccurances :: Int -> Int -> Int
countKOccurances x k =
    if x == 0 then 0
    else (if mod x 10 == k then 1 else 0) + countKOccurances(div x 10) k

--дали годината е високосна
isLeapYear :: Int -> Bool
isLeapYear year = 
    if(div year 4 * 4 == year && div year 100 * 100 /= year) ||
        (div year 400 * 400 == year)
        then True
        else False

--колко от числата от 1 до n удовлетворяват условието: i^3 + 13 * i * n + n^3 да е кратно на 5 или 9
countMultiplesOf5Or9 :: Int -> Int
countMultiplesOf5Or9 n = helper n
    where 
        helper i =
            if i < 1 then 0  
            else 
                let term = i^3 + 13 * i * n + n^3  
                in if (mod term 5 == 0) || (mod term 9 == 0)
                    then 1 + helper (i - 1)  
                    else helper (i - 1)  

--дали n е степен на k
isNPowerOfK :: Int -> Int -> Bool
isNPowerOfK n k =
    if n < 1 || k < 2 then False
    else if n == 1 then True
    else if mod n k /= 0 then False
    else isNPowerOfK (div n k) k

--дали n е съвършено число (т.е. сумата на делителите му без самото число е равна на n)
isPerfect :: Int -> Bool
isPerfect n = 
    if n <= 1 then False                 
    else isPerfectHelper n 1 0        

isPerfectHelper :: Int -> Int -> Int -> Bool
isPerfectHelper n i sumOfDivisors = 
    if i == n then sumOfDivisors == n    
    else if n `mod` i == 0 then          
        isPerfectHelper n (i + 1) (sumOfDivisors + i) 
    else 
        isPerfectHelper n (i + 1) sumOfDivisors 

listLength :: [Int] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

isNinList :: Int -> [Int] -> Bool
isNinList _ [] = False
isNinList n (x:xs) =
    if n == x then True
    else isNinList n xs

sumOfElem :: [Int] -> Int
sumOfElem [] = 0
sumOfElem (x:xs) = 
    x + sumOfElem xs

countNinList :: Int -> [Int] -> Int
countNinList _ [] = 0
countNinList n (x:xs) = 
    if n == x then 1 + countNinList n xs
    else countNinList n xs

indexOfN :: Int -> [Int] -> Int
indexOfN n (x:xs) = helper n (x:xs) 1
    where 
        helper _ [] _ = -1
        helper n (x:xs) pos
            | n == x    = pos
            | otherwise = helper n xs (pos + 1)

isSublist :: [Int] -> [Int] -> Bool
isSublist [] _ = True
isSublist (x:xs) list =
    elem x list && isSublist xs list

commonCount :: [Int] -> [Int] -> Int
commonCount [] _ = 0
commonCount (x:xs) list =
    if elem x list then 1 + commonCount xs list 
    else commonCount xs list

duplicatesExist :: [Int] -> Bool
duplicatesExist [] = False
duplicatesExist (x:xs) =
    if elem x xs then True
    else duplicatesExist xs

--takes first n elements
myTake :: Int -> [Int] -> [Int]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs)
    | n > 0 = x : myTake (n - 1) xs
    | otherwise = []

--removes first n elements
myDrop :: Int -> [Int] -> [Int]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (_:xs)
    | n > 0 = myDrop (n - 1) xs
    | otherwise = xs

--finds the smallest element
myMin :: [Int] -> Int
myMin [x] = x
myMin (x:xs) = min x (myMin xs)

--reverses a list
myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--unites two lists in list of pairs
myZip :: [a] -> [b] -> [a]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

--makes list of pairs into two lists
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y):xs) =
    let (l1, l2) = myUnzip xs
    in (x:l1, y:l2)

--concatenates lists
myConcat :: [[a]] -> [a]
myConcat [] = [] 
myConcat (x:xs) = x ++ myConcat xs

--checks if digits of number are in increasing order
isIncreasingDigits :: Int -> Bool
isIncreasingDigits x 
    | n < 10 = True
    | otherwise = lastDigit > secondLastDigit && isIncreasingDigits (n `div` 10)
    where
        lastDigit = n `mod` 10
        secondLastDigit = (n `div` 10) `mod` 10

--finds nth fibonacci number
--recursive version
nthFib :: Int -> Int
nthFib 0 = 0
nthFib 1 = 1
nthFib n = nthFib(n - 1) + nthFib(n - 2)

--iterative version
nthFib2 :: Int -> Int
nthFib2 n = helper 0 1 n 
    where
        helper a _ 0 = a
        helper a b counter = helper b (a + b) (counter - 1)

indexOf :: Int -> [Int] -> Int
indexOf n (x:xs) = helper n (x:xs) 1
    where
        helper _ [] _ = -1
        helper n (x:xs) pos
            | n == x    = pos
            | otherwise = helper n xs (pos + 1)

--transform string
offset :: Int
offset = ord 'a' - ord 'A'

isLowerLetter :: Char -> Bool
isLowerLetter ch = ord 'a' <= ord ch && ord ch <= ord 'z'
isUpperLetter :: Char -> Bool
isUpperLetter ch = ord 'A' <= ord ch && ord ch <= ord 'Z'

transformSymbol :: Char -> Char
transformSymbol ch
    | isLowerLetter ch = chr $ ord ch - offset
    | isUpperLetter ch = chr $ ord ch + offset
    | otherwise        = ' '

transformString :: String -> String
transformString [] = []
transformString (x:xs) = transformSymbol x : transformString xs

removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x:xs)  
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

takeFirstN :: [Int] -> [Int] -> ([Int], [Int])
takeFirstN [] _ = ([], [])
takeFirstN xs 0 = ([], xs)
takeFirstN (x:xs) n = (x:heads, tail)
    where
        (heads, tail) = takeFirstN xs (n-1)

simpleDecode :: [(Char, Int)] -> String
simpleDecode [] = ""
simpleDecode (x:xs) = helper x ++ simpleDecode xs
    where 
        helper (_, 0) = ""
        helper (ch, n) = ch : helper (ch, n - 1)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (x:xs) = lessOrEq ++ [x] ++ bigger
    where
        lessOrEq = quickSort [y | y <- xs, y <= x]
        bigger   = quickSort [y | y <- xs, y > x]

split :: [a] -> Int -> [[a]]
split [] _ = []
split xs n 
    | length heads < n = []
    | otherwise        = heads : split tail n
    where
        (heads, tail) = takeFirstN xs n

-- split :: [a] -> Int -> [[a]]
-- split [] _ = []
-- split xs n 
--     | length xs < n = []
--     | otherwise     = take n xs : split (drop n xs) n

normalizeSpaces :: String -> String
normalizeSpaces str = clean (words str) 
    where
        clean [] = ""
        clean [w] = w 
        clean (w:ws) = w ++ " " ++ clean ws

naturals = [2..]
firstNPrimes :: Int -> [Int]
firstNPrimes n = take n [x | x <- naturals, isPrime x]
    where
        isPrime x = [] == [div | div <- divisors, x `mod` div == 0]
            where divisors = [2..floor(sqrt(fromIntegral(x)))]

removeConsecutive :: [Int] -> [Int]
removeConsecutive (x:y:xs) =
    if x == y then removeConsecutive (y:xs)
    else x:removeConsecutive (y:xs)
removeConsecutive xs = xs

encode :: String -> [(Char, Int)]
encode [] = []
encode (x:xs) = helper x 1 xs
    where
        helper ch count [] = [(ch, count)]
        helper ch count (y:ys)
            | ch == y   = helper ch (count + 1) ys
            | otherwise = (ch, count) : helper y 1 ys

squareAll :: [Double] -> [Double]
squareAll = map (^2)

div3notdiv7 :: [Int] -> [Int]
div3notdiv7 [] = []
div3notdiv7 (x:xs) 
    | x `mod` 3 == 0 && x `mod` 7 /= 0 = x : div3notdiv7 xs
    | otherwise = div3notdiv7 xs

takeAllButKths :: [a] -> Int -> [(a, Int)]
takeAllButKths lst k = filter cond . zip lst $ [1..]
    where
        cond (_, idx) = idx `mod` k /= 0

split :: (a -> Bool) -> [a] -> ([a], [a])
split pred lst = (filter pred lst, filter (not . pred) lst)

calcAvgGrade :: [(String, Double)] -> Double
calcAvgGrade = average . map snd
    where average lst = sum lst / fromIntegral (length lst)
