{-
27.1. Да се дефинират функции над конфигурацията на везна, чрез които
може да се доавя тежест в лявата или дясната теглилка.

27.2. Функциите за добавяне на тежести да използват монада Maybe за трети-
ране на невалидни конфигурации. Считаме, че везната е в недвалидна

конфигурация, ако някоя от тежестите е над 100 или разликата на те-
жестите е над 50.

27.3. Да се илюстрира няколкократно добавяне на тежести чрез приложение
на оператора >>=.
-}

--ex1, 2 and 3
type Weight = Int

data Scale = Scale [Weight] [Weight]
  deriving Show

isValid :: Scale -> Bool
isValid (Scale left right) =
    all (<= 100) left && all (<= 100) right &&
    abs (sum left - sum right) <= 50

addLeft :: Weight -> Scale -> Maybe Scale
addLeft w (Scale l r)
  | w <= 0 = Nothing
  | otherwise =
      let newScale = Scale (w:l) r
      in if isValid newScale then Just newScale else Nothing

addRight :: Weight -> Scale -> Maybe Scale
addRight w (Scale l r)
  | w <= 0 = Nothing
  | otherwise =
      let newScale = Scale l (w:r)
      in if isValid newScale then Just newScale else Nothing
    
example :: Maybe Scale
example =
    Just (Scale [] [])
    >>= addLeft 20
    >>= addRight 10
    >>= addLeft 5
    >>= addRight 15

main :: IO ()
main = do
    let scale = Scale [2, 3] [5]
    print $ addLeft 4 scale       
    print $ addLeft (-1) scale   
    print $ addRight 100 scale    
    print $ addRight 1500 scale   

    print example