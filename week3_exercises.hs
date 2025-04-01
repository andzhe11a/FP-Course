-- ex1/a
firstEvenNumbers :: Int -> [Int]
firstEvenNumbers n = helper n 2
  where
    helper count current =
      if count == 0 then []
      else current : helper (count - 1) (current + 2)

-- ex1/b
arithmeticProgression :: Int -> Int -> Int -> [Int]
arithmeticProgression 0 _ _ = [] 
arithmeticProgression n a d = a : arithmeticProgression (n - 1) (a + d) d

-- ex1/c
factorial :: Int -> [Int]
factorial n = helper n
  where
    helper 0 = []  
    helper m = helper (m - 1) ++ [product [1..m]]  

-- ex1/d
allEvenNumbers :: [Int]
allEvenNumbers = generate 2
  where
    generate n = n : generate (n + 2)

-- ex1/e
allAritmethicProgression :: Int -> Int -> [Int]
allAritmethicProgression a d = generate a
  where
    generate x = x : generate (x + d)

-- ex1/f
infFactorial :: [Int]
infFactorial = generate 1 1
  where
    generate n fact = fact : generate (n + 1) (fact * (n + 1))

--ex2
digitsRightToLeft :: Int -> [Int]
digitsRightToLeft n = 
    if n == 0 then [] 
    else (n `mod` 10) : digitsRightToLeft (n `div` 10)

--ex3
digitsRightToLeftNoDuplicates :: Int -> [Int]
digitsRightToLeftNoDuplicates n = helper n []
  where
    helper num seen = 
        if num == 0 then []
        else 
            let digit = num `mod` 10
                rest = num `div` 10
            in if contains digit seen 
                then helper rest seen
                else digit : helper rest (digit : seen)
    
    contains x list =
        if list == [] then False
        else if head list == x then True
        else contains x (tail list)

--ex4
perfectNumbersUpTo :: Int -> [Int]
perfectNumbersUpTo n = findPerfect n
  where
    findPerfect x = 
        if x == 0 then []  
        else 
            if isPerfect x then x : findPerfect (x - 1)  
            else findPerfect (x - 1)

    isPerfect num = 
        if num == 1 then False
        else num == sumDivisors (num - 1) num 

    sumDivisors divisor num = 
        if divisor == 0 then 0  
        else 
            if num `mod` divisor == 0 then divisor + sumDivisors (divisor - 1) num
            else sumDivisors (divisor - 1) num

main :: IO ()
main = do
    print (firstEvenNumbers 5)  
    print (arithmeticProgression 5 3 2)  
    print (factorial 5) 
    print (take 10 allEvenNumbers)  
    print (take 10 (allAritmethicProgression 3 2))  
    print (take 10 infFactorial) 
    print (digitsRightToLeft 1234)
    print (digitsRightToLeftNoDuplicates 1223345)
    print (perfectNumbersUpTo 30)
