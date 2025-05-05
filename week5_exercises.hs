--ex1
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Read, Eq, Ord)

instance Show DayOfWeek where
  show Monday    = "Monday"
  show Tuesday   = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday  = "Thursday"
  show Friday    = "Friday"
  show Saturday  = "Saturday"
  show Sunday    = "Sunday"

dayToInt :: DayOfWeek -> Int
dayToInt Monday    = 1
dayToInt Tuesday   = 2
dayToInt Wednesday = 3
dayToInt Thursday  = 4
dayToInt Friday    = 5
dayToInt Saturday  = 6
dayToInt Sunday    = 7

intToDay :: Int -> DayOfWeek
intToDay 1 = Monday
intToDay 2 = Tuesday
intToDay 3 = Wednesday
intToDay 4 = Thursday
intToDay 5 = Friday
intToDay 6 = Saturday
intToDay 7 = Sunday
intToDay _ = error "Invalid day number"

prevDay :: DayOfWeek -> DayOfWeek
prevDay d = intToDay $ mod ((dayToInt d - 1) + 6) 7 + 1

nextDay :: DayOfWeek -> DayOfWeek
nextDay d = intToDay $ mod ((dayToInt d - 1) + 1) 7 + 1

daysInterval :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
daysInterval begin end =
  res ++ [nextDay (last res)]
  where
    daysOfWeek = [intToDay d | d <- [1..7]]
    res = takeWhile (/= end) (dropWhile (/= begin) (daysOfWeek ++ daysOfWeek))


--ex2
data Member
    = Student { facultyNumber :: Int, studentName :: String, studentCourses :: [String] }
    | Professor { professorName :: String, professorCourses :: [String], officeNumber :: Int }
    deriving (Show, Eq)

-- a)
countStudents :: [Member] -> Int
countStudents members = length [s | s@(Student _ _ _) <- members]

-- b)
attendees :: String -> [Member] -> [Member]
attendees course members =
    [s | s@(Student _ _ courses) <- members, course `elem` courses]

-- c)
classmembers :: String -> [Member] -> [Member]
classmembers course ms =
  [m | m <- ms, course `elem` getCourses m]
  where
    getCourses (Student _ _ cs) = cs
    getCourses (Professor _ cs _) = cs

-- d)
namestitles :: [Member] -> [String]
namestitles = map getName
  where
    getName (Student _ name _) = name
    getName (Professor name _ _) = "Prof. " ++ name

main :: IO ()
main = do
    -- ex1
    print $ show Tuesday                     
    print $ nextDay Sunday                  
    print $ prevDay Monday                
    print $ dayToInt Friday                 
    print $ intToDay 2                       
    print $ daysInterval Wednesday Saturday 
    print $ Monday < Friday    

    -- ex2
    let members = [
          Student 123 "Ivan" ["OOP", "FP"],
          Professor "Petrov" ["FP"] 101,
          Professor "Ivanova" ["DIS"] 101,
          Professor "Dimitrov" ["OOP"] 202,
          Student 456 "Maria" ["DIS"]]
    print $ countStudents members
    print $ attendees "DIS" members
    print $ classmembers "FP" members
    print $ namestitles members