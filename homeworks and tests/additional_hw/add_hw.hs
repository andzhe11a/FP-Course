--ex1
digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

allOddDigits :: Int -> Bool
allOddDigits n = all odd (digits n)

isReversible :: Int -> Bool
isReversible k =
  let str = show k
      revStr = reverse str
  in k /= 0 && head revStr /= '0' && allOddDigits (k + read revStr)

reversibleNumbers :: Int -> [Int]
reversibleNumbers n = [x | x <- [1..n], isReversible x]

--ex2
data BTree = Empty | Node Int BTree BTree 
    deriving (Eq, Show)

calcProduct :: BTree -> Int -> Int
calcProduct bt k = helper bt
  where
    helper Empty = 1  
    helper (Node val lt rt) =
      let lVal = getVal lt
          rVal = getVal rt
          productRest = helper lt * helper rt
      in if lVal + rVal > k
         then val * productRest
         else productRest

    getVal Empty = 0
    getVal (Node v _ _) = v

main :: IO ()
main = do
--ex1
    print $ reversibleNumbers 20  
    print $ reversibleNumbers 31  
    print $ reversibleNumbers 10  

--ex2
    let bt = Node 5 
                (Node 1 (Node 5 (Node 1 Empty Empty) 
                                (Node 2 Empty Empty))
                        (Node 2 (Node 3 Empty Empty) 
                                (Node 4 Empty Empty)))
                (Node 2 (Node 1 (Node 1 Empty Empty) 
                                (Node 7 Empty Empty))
                        (Node 9 (Node 5 Empty Empty) 
                                (Node 2 Empty Empty)))
    
    print $ calcProduct bt 2
    print $ calcProduct bt 6 
    print $ calcProduct bt 7 
    print $ calcProduct bt 8 
