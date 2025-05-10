--task 1
import Data.List (nub)

--a)
commonCount :: Eq a => [a] -> [a] -> Int
commonCount xs ys = length [x | x <- xs, x `elem` ys]
--b)
commonCountUnique :: Eq a => [a] -> [a] -> Int
commonCountUnique xs ys = length [x | x <- nub xs, x `elem` nub ys]


--task 2
people = [Person {name = "Ivan",  birthyear = 1999, height = 185},

        Person {name = "Petar", birthyear = 2002, height = 179},

        Person {name = "Maria", birthyear = 2003, height = 182}]
--a)
data Person = Person { name :: String, birthyear :: Int, height :: Int }
  deriving (Show, Eq)
--b)
averageHeight :: [Person] -> Double
averageHeight people =
  let totalHeight = sum (map height people)
      count = length people
  in fromIntegral totalHeight / fromIntegral count
--c)
findHigherThanAverage :: [Person] -> [Person]
findHigherThanAverage people =
  let avgHeight = averageHeight people
  in filter (\person -> fromIntegral (height person) > avgHeight) people


--task 3 
isFixedPoint :: Eq a => [a -> a] -> a -> Bool
isFixedPoint fs x = any (\f -> f x == x) fs


--task 4
isSquareMatrix :: [[a]] -> Bool
isSquareMatrix [] = False
isSquareMatrix m =
  let n = length m
  in n >= 2 && all (\row -> length row == n) m


main :: IO ()
main = do

    --task 1
    --a)
    let list1 = [1, 2, 3, 4]
    let list2 = [2, 3, 5, 6]

    print $ commonCount list1 list2  

    --b)
    let list3 = [1, 2, 2, 3, 4]
    let list4 = [2, 3, 3, 5, 1]

    print $ commonCountUnique list3 list4


    --task 2
    --b)
    print $ averageHeight people

    --c)
    print $ findHigherThanAverage people


    --task 3
    let functions = [(+2), (*2), (^2)]
    let x = 2

    print $ isFixedPoint functions x


    --task 4
    let matrix = [[1,2], [3,4]]
    let nonSquareMatrix = [[1,2,3], [4,5,6]]

    print $ isSquareMatrix matrix
    print $ isSquareMatrix nonSquareMatrix