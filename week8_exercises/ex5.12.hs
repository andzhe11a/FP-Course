{-
5.12. При условията на дефинициите от предишната задача, да се дефинира
функция bool connected(), която проверява дали от всеки елемент на
матрицата (sx, sy) до всеки елемент на матрицата (tx, ty), такива, че
sx ≤ tx и sy ≤ ty, съществува низходящ път.
-}

type Matrix = [[Int]]
type Coord = (Int, Int)

isPassable :: Matrix -> Coord -> Bool
isPassable m (x, y) =
  x >= 0 && y >= 0 && x < length m && y < length (head m) && (m !! x !! y == 0)

downrightNeighbors :: Coord -> [Coord]
downrightNeighbors (x, y) = [(x + 1, y), (x, y + 1)]

dfs :: Matrix -> Coord -> Coord -> [Coord] -> Bool
dfs m current target visited
  | current == target = True  
  | current `elem` visited = False  
  | otherwise =
      any (\next -> isPassable m next && dfs m next target (current : visited))
          (downrightNeighbors current)

--ex12 (addition to ex11)
connected :: Matrix -> Bool
connected m =
  all (\(sx, sy) -> all (\(tx, ty) -> dfs m (sx, sy) (tx, ty) []) validCoords) validCoords
  where
    validCoords = [(x, y) | x <- [0..length m - 1], y <- [0..length (head m) - 1], isPassable m (x, y)]

testMaze :: Matrix
testMaze =
  [ [0, 0, 0, 0],
    [0, 1, 0, 1],
    [0, 0, 0, 0],
    [1, 0, 1, 0]
  ]

main :: IO ()
main = do
  print $ connected testMaze  