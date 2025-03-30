--ex1/a
areaByBaseHeight :: Double -> Double -> Double
areaByBaseHeight base height = (base * height) / 2

--ex1/b
areaByThreeSides :: Double -> Double -> Double -> Double
areaByThreeSides a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

--ex2
quadrant :: (Double, Double) -> String
quadrant (x, y) =
    if x == 0 && y == 0 then "Center"
    else if x == 0 then "On Y"
    else if y == 0 then "On X"
    else if x > 0 && y > 0 then "First quadrant"
    else if x < 0 && y > 0 then "Second quadrant"
    else if x < 0 && y < 0 then "Third quadrant"
    else "Fourth quadrant"

--ex3/a
isDivisibleBy4Or7 :: Int -> Bool
isDivisibleBy4Or7 p = (mod p 4 == 0) || (mod p 7 == 0)

--ex3/b
noRealRoots :: Double -> Double -> Double -> Bool
noRealRoots a b c = (b^2 - 4 * a * c) < 0

--ex3/c
isInsideCircle :: Double -> Double -> Bool
isInsideCircle a b = (a^2 + (b - 1)^2) < 25

--ex3/d
isOutsideCircle :: Double -> Double -> Double -> Double -> Double -> Bool
isOutsideCircle a b c d f = (a - c)^2 + (b - d)^2 > f^2

--ex3/e
isInsideThirdQuadrantAndCircle :: Double -> Double -> Bool
isInsideThirdQuadrantAndCircle a b = a < 0 && b < 0 && (a^2 + b^2) < 25

--ex3/g
isInSegment :: Double -> Bool
isInSegment x = x >= 0 && x <= 1

--ex3/h
isEqualToMax :: Int -> Int -> Int -> Int -> Bool
isEqualToMax x a b c = x == max a (max b c)

--ex3/i
isNotEqualToMax :: Int -> Int -> Int -> Int -> Bool
isNotEqualToMax x a b c = x /= max a (max b c)

--ex3/j
nonePositive :: Int -> Int -> Int -> Bool
nonePositive a b c = a <= 0 && b <= 0 && c <= 0

--ex3/k
contains7 :: Int -> Bool
contains7 p = p > 99 && p < 1000 &&
            mod p 10 == 7 ||
            mod (div p 10) 10 == 7 ||
            div p 100 == 7

--ex3/l
allDigitsAreDifferent :: Int -> Bool
allDigitsAreDifferent m = 
    let a = div m 100
        b = mod (div m 10) 10
        c = mod m 10
    in m > 99 && m < 1000 &&
    a /= b && b /= c && a /= c

--ex3/m
areAtLeastTwoDigitsEqual :: Int -> Bool
areAtLeastTwoDigitsEqual m = 
    let a = div m 100
        b = mod (div m 10) 10
        c = mod m 10
    in m > 99 && m < 1000 &&
    (a == b || b == c || a == c)

--ex3/n
isStrictlyMonotonic :: Int -> Bool
isStrictlyMonotonic x = 
    let a = div x 100
        b = mod (div x 10) 10
        c = mod x 10
    in x > 99 && x < 1000 &&
    ((a < b && b < c) || (a > b && b > c))

--ex4
countDigits :: Int -> Int
countDigits n = 
    if n < 10
    then 1
    else 1 + countDigits (div n 10)

--ex5
sumDigits :: Int -> Int
sumDigits n = 
    if n < 10
    then n 
    else mod n 10 + sumDigits (div n 10)

--ex6
powFunc :: Int -> Int -> Int
powFunc x k =
    if k == 0
    then 1
    else x * powFunc x (k - 1)

--ex7/a
containsK :: Int -> Int -> Bool
containsK x k = 
    if x == 0
    then False
    else if mod x 10 == k 
        then True 
        else containsK (div x 10) k

--ex7/b
countKOccurances :: Int -> Int -> Int
countKOccurances x k =
    if x == 0 then 0
    else (if mod x 10 == k then 1 else 0) + countKOccurances(div x 10) k

--ex8
isLeapYear :: Int -> Bool
isLeapYear year = 
    if(div year 4 * 4 == year && div year 100 * 100 /= year) ||
        (div year 400 * 400 == year)
        then True
        else False

--ex9
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

--ex10
isNPowerOfK :: Int -> Int -> Bool
isNPowerOfK n k =
    if n < 1 || k < 2 then False
    else if n == 1 then True
    else if mod n k /= 0 then False
    else isNPowerOfK (div n k) k

--ex11
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

main :: IO ()
main = do
    print (areaByBaseHeight 5 10)
    print (areaByThreeSides 3 4 5)

    print (quadrant (0, 0))  
    print (quadrant (0, 5))   
    print (quadrant (4, 0))   
    print (quadrant (3, 4))    
    print (quadrant (-2, 6))   
    print (quadrant (-5, -3))  
    print (quadrant (7, -8))

    print (isDivisibleBy4Or7 8)    
    print (isDivisibleBy4Or7 14)     
    print (isDivisibleBy4Or7 5)      

    print (noRealRoots 1 2 3)      
    print (noRealRoots 1 5 4)      

    print (isInsideCircle 3 2)       
    print (isInsideCircle 6 1)   

    print (isOutsideCircle 7 7 0 0 5)  
    print (isOutsideCircle 2 3 0 0 5)  

    print (isInsideThirdQuadrantAndCircle (-3) (-3))  
    print (isInsideThirdQuadrantAndCircle (-6) (-6))  
    print (isInsideThirdQuadrantAndCircle 3 (-3))

    print (isInSegment 0.5)   
    print (isEqualToMax 7 5 7 3)     
    print (isNotEqualToMax 5 5 7 3)  
    print (nonePositive (-1) 0 (-3))  
    print (contains7 172)    
    print (allDigitsAreDifferent 123)  
    print (allDigitsAreDifferent 112)  

    print (areAtLeastTwoDigitsEqual 122)  
    print (areAtLeastTwoDigitsEqual 123)  
    print (isStrictlyMonotonic 123)   
    print (isStrictlyMonotonic 132)

    print (countDigits 12345)

    print (sumDigits 1234)

    print (powFunc 2 3)

    print (isLeapYear 2024)

    print (countMultiplesOf5Or9 10)

    print (isNPowerOfK 27 3)

    print (isPerfect 6)
