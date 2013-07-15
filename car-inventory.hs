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
  putStrLn $ "**********************************************\n"                                 ++
             "*        Air Capital Automobiles             *\n"                                 ++
             "*      (inventory tracking system)           *\n"                                 ++
             "**********************************************\n\n"                               ++
             "Please choose from the following options:\n\n"                                    ++
             "______________________________________________\n"                                 ++
             show optionAddCar             ++ "  -  Add a new car to inventory.\n\n"            ++
             show optionRemoveCar          ++ "  -  Remove a sold car from inventory.\n\n"      ++
             show optionDisplayCar         ++ "  -  Display info on car. (inv # needed)\n\n"    ++
             show optionDisplayCarsByYear  ++ "  -  Display cars of specific Year.\n\n"         ++
             show optionDisplayCarsByPrice ++ "  -  Display cars within a price range.\n\n"     ++
             show optionDisplayCarsByColor ++ "  -  Display cars of specified color.\n\n"       ++
             show optionDisplayAllCars     ++ "  -  Display All Cars in inventory.\n\n\n"       ++
             show optionQuit               ++ "  -  Quit. (exit inventory tracking system)\n\n" ++
             "______________________________________________"

promptUser :: String -> IO String
promptUser message = do
  putStr message
  hFlush stdout
  response <- getLine
  return response

-- returns an option
-- reprompts user if input is invalid (unless it's non-numeric, not sure how to deal with that)
getOption :: IO Integer
getOption = do
  option <- promptUser optionPrompt
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
      _vinNumber <- promptUser "Enter inventory number of the car to display: "
      displayCars $ filter (\car -> _vinNumber == vinNumber car) cars
      return (cars, False)
  | option == optionDisplayCarsByYear = do
      _year <- promptUser "Enter the year of the car you are searching for: "
      displayCars $ filter (\car -> year car == read(_year)) cars
      return (cars, False)
  | option == optionDisplayCarsByPrice = do
      low  <- promptUser "Enter the low end of the price range to search: "
      high <- promptUser "Enter the low end of the price range to search: "
      displayCars $ filter (\car -> (read low <= price car) && (price car <= read high)) cars
      return (cars, False)
  | option == optionDisplayCarsByColor = do
      _color <- promptUser "Enter the color of the car you are searching for: "
      displayCars $ filter (\car -> color car == _color) cars
      return (cars, False)
  | option == optionDisplayAllCars = do
      displayCars cars
      return (cars, False)
  | option == optionQuit = do
      putStrLn "Goodbye!"
      return (cars, True)

displayCars :: [Car] -> IO ()
displayCars [] = do return ()
displayCars (car:remainingCars) = do
  displayCar car
  displayCars remainingCars
  where displayCar car = putStrLn $ show car

readCars :: String -> IO [Car]
readCars filename = do
  carFileData <- readFile filename
  return $ map carFromData (carData $ lines carFileData)
  where
    carData carFileData
      | length carFileData < 8 = []
      | otherwise              = [(take 8  $ fst $ splitAt 9 carFileData)] ++
                                  (carData $ snd $ splitAt 9 carFileData)
    carFromData [line1, style, color, miles, ac, vin, price, prevOwner] =
      Car { year            = read  ((words line1) !! 0)
          , make            =        (words line1) !! 1
          , model           = unwords $ drop 2 (words line1)
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
