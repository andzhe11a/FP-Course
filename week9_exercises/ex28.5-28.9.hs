{-
28.5. С помощта на zipWith да се дефинира функция sums :: [Int] -> [Int],
която по списък от числа L = l1, l2, l3, ... намира списъка S = l1,(l1 + l2) ,(l1 + l2 + l3), ....

28.6. Да се дефинира функция separate :: (a->Bool) -> [a] -> ([a],[a]),
която по предикат p и списък l връща двойката (pref,suf). pref е най-
дългият възможен префикс, такъв че всички негови елементи увовлет-
воряват p. suf е останалата част от списъка l.

28.7. Да се дефинира функция split :: (a->Bool) -> [a] -> [[a]], получ-
ваваща предикат p и списък l. Елементите на l, удовлетворяващи p, се
считат за “разделители” в l и списъкът се разделя на части, обособени
от тези разделители. 

28.8. С помощта на map, mapMaybe, filter и fold да се намерят:
а) Броя на препятсвията в даден лабиринт
б) Броя съседи на целта, които не са препятсвия
в) Дали целта е оградена изцяло от препятствия (True или False)

28.9. (*) С помощта на foldr да се дефинира функция, която проверява да-
ли дали даден списък от числа l::[Int] е нареден във възходящ ред.
-}

--ex5
sums :: [Int] -> [Int]
sums [] = []
sums (x:xs) = zipWith (+) (x : (accumulateSums xs 0)) (x : xs)
  where
    accumulateSums :: [Int] -> Int -> [Int]
    accumulateSums [] _ = []
    accumulateSums (y:ys) acc = (y + acc) : accumulateSums ys (acc + y)

--ex6
separate :: (a -> Bool) -> [a] -> ([a], [a])
separate p = break (not . p)

--ex7
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p xs =
  let (part, rest) = break p xs
  in part : case rest of
               [] -> []
               (_:rest') -> split p rest'

--ex8
type Matrix = [[Int]]
type Coord = (Int, Int)

maze :: Matrix
maze =
  [ [0, 1, 0],
    [1, 0, 1],
    [0, 1, 0]
  ]

--a)
countObstacles :: Matrix -> Int
countObstacles = sum . map (length . filter (== 1))

--b)
neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

isInBounds :: Matrix -> Coord -> Bool
isInBounds m (x, y) = x >= 0 && y >= 0 && x < length m && y < length (head m)

isPassable :: Matrix -> Coord -> Bool
isPassable m (x, y) = isInBounds m (x, y) && (m !! x !! y == 0)

countPassableNeighbors :: Matrix -> Coord -> Int
countPassableNeighbors m c = length $ filter (isPassable m) (neighbors c)

--c)
isSurrounded :: Matrix -> Coord -> Bool
isSurrounded m c = all (\n -> not (isPassable m n)) (neighbors c)

--ex9
isAscending :: [Int] -> Bool
isAscending [] = True
isAscending (x:xs) =
    fst $ foldr
        (\curr (ok, next) -> (ok && curr <= next, curr))
        (True, last (x:xs)) 
        (x:xs)

main :: IO ()
main = do
    --ex5
    print $ sums [1, 2, 3, 4]  

    --ex6
    print $ separate even [2,4,6,7,8,10]

    --ex7
    print $ split (== ',') "part1,part 2,part3"

    --ex8
    print $ countObstacles maze                         
    print $ countPassableNeighbors maze (1,1)              
    print $ isSurrounded maze (1,1)

    --ex9                      
    print $ isAscending [1, 2, 3, 4, 5]    
    print $ isAscending [1, 3, 2, 4, 5]    