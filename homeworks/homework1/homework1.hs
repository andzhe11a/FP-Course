--ex1
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = encodeHelper x 1 xs
    where
        encodeHelper current count [] = [(count, current)]
        encodeHelper current count (y:ys) =
            if current == y
                then encodeHelper current (count + 1) ys
                else (count, current) : encodeHelper y 1 ys
--ex2
mergeEvenOdd :: [Int] -> [Int] -> [Int]
mergeEvenOdd as [] = as
mergeEvenOdd [] bs = bs
mergeEvenOdd (a:as) (b:bs) = a : b : mergeEvenOdd as bs

main :: IO ()
main = do
    print (encode [1,1,1,2,2,3,4,4,4,4])
    print (encode [1,2,2,3,3,4,4])
    print (encode [1,2,1,2,2,1,1])

    print (mergeEvenOdd [1, 2, 3] [4, 5, 6])  
    print (mergeEvenOdd [1, 2] [6])  --[6..]
    print (mergeEvenOdd [1, 2] [6, 7, 8])
