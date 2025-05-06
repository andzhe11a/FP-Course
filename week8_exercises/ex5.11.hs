{-
5.11. Нека е дадена квадратна матрица от цели числа N × N, представяща
“лабиринт”. Елементи на матрицата със стойност 0 смятаме за “прохо-
дими”, а всички останали - за “непроходими”. Път в лабиринта нарича-
ме всяка последователност от проходими елементи на матрицата, които
са съседни вертикално или хоризонтално, такава че (1) никой елемент
от последователността не е последван директно от предшественика си
(забранено е “връщането назад”) и (2) най-много един елемент на после-
дователността се среща в нея повече от веднъж (има най-много един
“цикъл”).

Да се дефинира функция bool downstairs (int sx, int sy, int tx,
int ty), която проверява дали съществува път от елемента (sx, sy) до
елемента (tx, ty), такъв, че всеки следващ елемент от пътя е или вдясно,
или под предишния. Такъв път да наричаме “низходящ”.
Пример: На Фигура 1.8(а) такъв път съществува от елемента (0, 2) до
елемента (3, 3), но не и от (3, 1) до (0, 0).
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

downstairs :: Matrix -> Coord -> Coord -> Bool
downstairs m start target =
  isPassable m start && isPassable m target && dfs m start target []

testMaze :: Matrix
testMaze =
  [ [0, 0, 0, 0],
    [0, 1, 0, 1],
    [0, 0, 0, 0],
    [1, 0, 1, 0]
  ]

main :: IO ()
main = do
  print $ downstairs testMaze (0, 0) (3, 3)
  print $ downstairs testMaze (3, 1) (0, 0)
  print $ downstairs testMaze (0, 0) (2, 2)