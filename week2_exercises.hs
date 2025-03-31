--ex1
myLength :: [Int] -> Int
myLength [] = 0
myLength list = 1 + myLength (tail list)

myElemCheck :: Int -> [Int] -> Bool
myElemCheck _ [] = False
myElemCheck x list = (head list == x) || myElemCheck x (tail list)

mySum :: [Int] -> Int
mySum [] = 0
mySum list = head list + mySum (tail list)

--ex2
countXinList :: Int -> [Int] -> Int
countXinList _ [] = 0
countXinList x list = if head list == x then 1 + countXinList x (tail list) else countXinList x (tail list)

--ex3
index :: Int -> [Int] -> Int
index x list = helper x list 1
    where
        helper _ [] _ = -1  
        helper x list pos =
            if head list == x 
            then pos  
            else helper x (tail list) (pos + 1) 

--ex4
--using the myElemCheck function
common :: [Int] -> [Int] -> Int
common [] _ = 0
common l1 l2 = if myElemCheck (head l1) l2
                then 1 + common (tail l1) l2
                else common (tail l1) l2

--ex5
--using the myElemCheck function 
duplicates :: [Int] -> Bool
duplicates [] = False
duplicates list = if myElemCheck (head list) (tail list)
                    then True
                    else duplicates (tail list)

main :: IO ()
main = do 
    print (myLength [2, 3, 4])
    print (myElemCheck 2 [2, 3, 4])
    print (mySum [2, 3, 4])

    print (countXinList 3 [3, 4, 3, 5])

    print (index 7 [1, 2, 7, 3, 2])

    print (common [1, 2, 3] [1, 2, 3, 4, 5])

    print (duplicates [1, 2, 3, 4, 1])
