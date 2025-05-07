-- Basic tasks for higher-order functions

-- Function to show the lengths of an even list and an odd list
countEvenAndOddList :: [Int] -> (Int, Int) 
countEvenAndOddList xs = (length evenList, length oddList)
  where
    evenList = filter even xs
    oddList = filter odd xs

-- Function to calculate the double of each element in a list
doubleList :: [Int] -> [Int]
doubleList xs = map (*2) xs
doubleList' :: [Int] -> [Int]  
doubleList' xs = [x * 2 | x <- xs]

-- Function to check if each element in a list is even
isEvenList :: [Int] -> [Bool]
isEvenList xs = map even xs
isEvenList' :: [Int] -> [Bool]
isEvenList' xs = [even x | x <- xs]     

-- Function to show the lengths of each word in a list of strings
wordLengths :: [String] -> [Int]
wordLengths xs = [length x | x <- xs]
wordLengths' :: [String] -> [Int]
wordLengths' xs = map length xs

-- Function to filter even numbers from a list
filterEven :: [Int] -> [Int]    
filterEven xs = filter even xs

-- Function to filter positive numbers from a list
filterPositive :: [Int] -> [Int]
filterPositive xs = filter (>0) xs

-- Function to filter the words that start with an uppercase letter from a list of strings
startsWithUppercase :: [String] -> [String]
startsWithUppercase xs = filter (\x -> head x `elem` ['A'..'Z']) xs
startsWithUppercase' :: [String] -> [String]    
startsWithUppercase' xs = filter (\x -> isUpper (head x)) xs

-- Function that coompares two lists of integers and returns True if they are equal
compareLists :: [Int] -> [Int] -> Bool
compareLists xs ys = all (\(x, y) -> x == y) (zip xs ys)

-- Function that sums two lists of integers element-wise
sumLists :: [Int] -> [Int] -> [Int]
sumLists xs ys = zipWith (+) xs ys

-- Fuunctions that removes consecutive duplicates from a list
removeConsecutive :: [Int] -> [Int]
removeConsecutive xs = [x | (x, y) <- zip xs (tail xs), x /= y] ++ [last xs]

-- Function that removes every kth element from a list
takeAllButKths :: [a] -> Int -> [a]
takeAllButKths xs k = [x | (x, i) <- zip xs [0..], (i + 1) `mod` k /= 0]

-- Function that checks if integers a, b, and c can form a triangle
formsTriangle :: Int -> Int -> Int -> Bool
formsTriangle a b c = a + b > c && a + c > b && b + c > a

-- Function that applies a function n times to a value
applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes f 0 x = x
applyNTimes f n x = applyNTimes f (n - 1) (f x)

-- Functions that finds the minimum value of a list of functions applied to an integer
findMinValue :: [(Int -> Int)] -> Int -> Int
findMinValue fs x = minimum [f x | f <- fs]