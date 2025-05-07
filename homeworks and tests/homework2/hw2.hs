type Species = String
type AnimalId = Int

data Diet = Herbivore | Carnivore | Omnivore 
    deriving (Show, Eq)

data Animal = Animal { species :: Species, animalId :: AnimalId,
                       diet :: Diet, hungerLevel :: Int } deriving Eq

--1)
instance Show Animal where
  show (Animal s id d hunger) =
    s ++ "(" ++ show id ++ "," ++ show d ++ "," ++ show hunger ++ ")"

--2
updateDiet :: [Animal] -> AnimalId -> Diet -> [Animal]
updateDiet zoo id d =
  [if animalId a == id then a { diet = d } else a | a <- zoo]

--3)
feedAnimal :: [Animal] -> Int -> AnimalId -> [Animal]
feedAnimal zoo k id =
  map (\ a -> if animalId a == id
              then a { hungerLevel = hungerLevel a + k }
              else a)
      zoo

--4)
feedIfHungry :: [Animal] -> Int -> [AnimalId] -> [Animal]
feedIfHungry zoo k =
  foldr (\ id zoo -> if elem id hungryAnimals
                        then feedAnimal zoo k id
                        else zoo)
        zoo
  where
    hungryAnimals =
      [animalId a | a <- zoo, hungerLevel a < 5]

--5)
findAnimalById :: [Animal] -> AnimalId -> Maybe Animal
findAnimalById []     _  = Nothing
findAnimalById (a:as) id =
  if animalId a == id then Just a
  else findAnimalById as id

main :: IO()
main = do
  print $ "Task 2"
  print $ Animal "Lion" 1 Carnivore 3 
  print $ updateDiet zoo 3 Herbivore 
  print $ feedAnimal zoo 3 1 
  print $ feedIfHungry zoo 3 [1,2,3,4]
  print $ feedIfHungry zoo 3 [3] 
  print $ findAnimalById zoo 10 
  print $ findAnimalById zoo 2 
  where zoo = [ Animal "Lion" 1 Carnivore 3
              , Animal "Elephant" 2 Herbivore 3
              , Animal "Bear" 3 Omnivore 6
              , Animal "Turtle" 4 Herbivore 2]