{-
26.1. Да се дефинира тип DayOfWeek, представящ ден от седмицата. За типа
да се реализират възможност за:
• Въвеждане и извеждане
• Сравнение на дни с ==,<,>
• Намиране на следващ и предишен ден от седмицата
• Преобразуване от и до число
• Създаване на интервал от дни
Съответните възможности да се добавят по два начина:
• Чрез нарочни функции за целта
• Чрез наследяване на базови класове

26.2. В един университет има студенти и преподаватели. Всеки студент има
факултетен номер, име и списък от курсове, които посещава. Всеки пре-
подавател има име, списък от курсове, които преподава и номер на каби-
нет. Да се дефинира алгебричен тип Member със съответни конструктори
за студент и преподавател.
а) По списък [Member] да се намери броят на студентите.
б) Да се дефинира функция attendees :: String -> [Member] -> [Member],
която по име на предмент и списък от членове на унивеситета на-
мира броя на студентите, които слушат предмета.
в) Да се дефинира функция classmembers :: String -> [Member] ->
[Member], която по име на предмент и списък от членове на уни-
веситета намира списък само на тези членове, които преподат или
слушат дадения предмет.
г) Да се дефинира функция namestitles :: [Member] -> [String],
която връща имената на всички членове на университета, като пред
името на всеки преподавател автоматично добавя титлата “проф.”.
д) (*) Да се дефинира функция bussiest :: [Member] -> Int, която
намира кабинета с най-много преподаватели.
-}

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