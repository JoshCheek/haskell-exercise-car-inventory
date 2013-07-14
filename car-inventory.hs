import System.IO (hFlush, stdout, readFile)

data Car = Car { year           :: Integer
               , make           :: String
               , model          :: String
               , style          :: String
               , color          :: String
               , mileage        :: String
               , airConditioner :: Bool
               , vinNumber      :: String
               , price          :: Double
               , previousOwner  :: String
               } deriving Show

optionAddCar             = 1 :: Integer
optionRemoveCar          = 2 :: Integer
optionDisplayCar         = 3 :: Integer
optionDisplayCarsByYear  = 4 :: Integer
optionDisplayCarsByPrice = 5 :: Integer
optionDisplayCarsByColor = 6 :: Integer
optionDisplayAllCars     = 7 :: Integer
optionQuit               = 8 :: Integer
options                  = [optionAddCar, optionRemoveCar, optionDisplayCar, optionDisplayCarsByYear, optionDisplayCarsByPrice, optionDisplayCarsByColor, optionDisplayAllCars, optionQuit]
optionPrompt             = "Enter your choice (1-8) ===> "

printMenu :: IO ()
printMenu = do
  putStrLn "**********************************************"
  putStrLn "*        Air Capital Automobiles             *"
  putStrLn "*      (inventory tracking system)           *"
  putStrLn "**********************************************"
  putStrLn ""
  putStrLn "Please choose from the following options:"
  putStrLn  ""
  putStrLn "______________________________________________"
  putStrLn $ (show optionAddCar)             ++ "  -  Add a new car to inventory."
  putStrLn ""
  putStrLn $ (show optionRemoveCar)          ++ "  -  Remove a sold car from inventory."
  putStrLn ""
  putStrLn $ (show optionDisplayCar)         ++ "  -  Display info on car. (inv # needed)"
  putStrLn ""
  putStrLn $ (show optionDisplayCarsByYear)  ++ "  -  Display cars of specific Year."
  putStrLn ""
  putStrLn $ (show optionDisplayCarsByPrice) ++ "  -  Display cars within a price range."
  putStrLn ""
  putStrLn $ (show optionDisplayCarsByColor) ++ "  -  Display cars of specified color."
  putStrLn ""
  putStrLn $ (show optionDisplayAllCars)     ++ "  -  Display All Cars in inventory."
  putStrLn ""
  putStrLn ""
  putStrLn $ (show optionQuit)               ++ "  -  Quit. (exit inventory tracking system)"
  putStrLn "______________________________________________"

-- returns an option
-- reprompts user if input is invalid (unless it's non-numeric, not sure how to deal with that)
getOption :: IO Integer
getOption = do
  putStr optionPrompt
  hFlush stdout
  option <- getLine
  if elem (read option) options
    then return (read option)
    else getOption

handleOption :: [Car] -> Integer -> IO ([Car], Bool)
handleOption cars option
  | option == optionAddCar = do
      putStrLn "ADD A NEW CAR"
      return (cars, False)
  | option == optionRemoveCar = do
      putStrLn "REMOVE CAR"
      return (cars, False)
  | option == optionDisplayCar = do
      putStrLn "DISPLAY CAR"
      return (cars, False)
  | option == optionDisplayCarsByYear = do
      putStrLn "DISPLAY CAS BY YEAR"
      return (cars, False)
  | option == optionDisplayCarsByPrice = do
      putStrLn "DISPLAY CARS BY PRICE"
      return (cars, False)
  | option == optionDisplayCarsByColor = do
      putStrLn "DISPLAY CARS BY COLOUR"
      return (cars, False)
  | option == optionDisplayAllCars = do
      putStrLn "DISPLAY ALL CARS"
      return (cars, False)
  | option == optionQuit = do
      putStrLn "QUITTING"
      return (cars, True)

readCars :: String -> IO [Car]
readCars filename = do
  carFileData <- readFile filename
  return $ map carFromData (carData $ lines carFileData)
  where
    carData carFileData
      | length carFileData < 9 = []
      | otherwise              = [(take 8  $ fst $ splitAt 9 carFileData)] ++
                                  (carData $ snd $ splitAt 9 carFileData)
    carFromData [line1, style, color, miles, ac, vin, price, prevOwner] =
      Car { year            = read ((words line1) !! 0)
          , make            =       (words line1) !! 1
          , model           =       (words line1) !! 2
          , style           = style
          , color           = color
          , mileage         = miles
          , airConditioner  = if ac == "Y" then True else False
          , vinNumber       = vin
          , price           = read price
          , previousOwner   = prevOwner
          }

menuLoop :: [Car] -> IO ()
menuLoop cars = do
  printMenu
  option <- getOption
  (newCars, shouldQuit) <- handleOption cars option
  if shouldQuit
    then return ()
    else menuLoop newCars

main :: IO ()
main = do
  cars <- readCars "inventory.data"
  menuLoop cars
