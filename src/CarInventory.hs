import Car.Car
import System.IO (hFlush, stdout, readFile, writeFile)
import System.Directory (renameFile)
import Safe (readDef, readMay)
import Data.Aeson

optionAddCar             = 1  :: Integer
optionRemoveCar          = 2  :: Integer
optionDisplayCar         = 3  :: Integer
optionDisplayCarsByYear  = 4  :: Integer
optionDisplayCarsByPrice = 5  :: Integer
optionDisplayCarsByColor = 6  :: Integer
optionDisplayAllCars     = 7  :: Integer
optionToJson             = 8  :: Integer
optionQuit               = 9  :: Integer
optionRepeat             = 10 :: Integer
options                  = [optionAddCar, optionRemoveCar, optionDisplayCar, optionDisplayCarsByYear, optionDisplayCarsByPrice, optionDisplayCarsByColor, optionDisplayAllCars, optionToJson, optionQuit, optionRepeat]
optionPrompt             = "Enter your choice (1-9) ===> "
inventoryFilename        = "inventory.data"

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
             show optionDisplayAllCars     ++ "  -  Display All Cars in inventory.\n\n"         ++
             show optionToJson             ++ "  -  Display All Cars as JSON.\n\n\n"            ++
             show optionQuit               ++ "  -  Quit. (exit inventory tracking system)\n\n" ++
             "______________________________________________"

promptUserString :: String -> IO String
promptUserString message = do
  putStr message
  hFlush stdout
  getLine

promptUserInteger :: String -> IO Integer
promptUserInteger message = do
  putStr message
  hFlush stdout
  response <- getLine
  case readMay response of
    Just value -> return value
    Nothing    -> promptUserInteger message

promptUserDouble :: String -> IO Double
promptUserDouble message = do
  putStr message
  hFlush stdout
  response <- getLine
  case readMay response of
    Just value -> return value
    Nothing    -> promptUserDouble message

getOption :: IO Integer
getOption = do
  option <- promptUserString optionPrompt
  if elem (readDef optionRepeat option) options
    then return (readDef optionRepeat option)
    else getOption

promptCar :: IO Car
promptCar = do
  putStrLn $  "Input data for new car:\n" ++
              "________________________________\n\n"
  _year      <- promptUserInteger "Input year: "
  _make      <- promptUserString  "Input make: "
  _model     <- promptUserString  "Input model: "
  _style     <- promptUserString  "Input style: "
  _color     <- promptUserString  "Input color: "
  _miles     <- promptUserString  "Input mileage: "
  _ac        <- promptUserString  "Input air conditioner (y/n): "
  _vin       <- promptUserString  "Input inventory number: "
  _price     <- promptUserDouble  "Input price: "
  _prevOwner <- promptUserString  "Input previous owner: "
  return Car { year            = _year
             , make            = _make
             , model           = _model
             , style           = _style
             , color           = _color
             , mileage         = _miles
             , airConditioner  = if _ac == "Y" || _ac == "y" then True else False
             , vinNumber       = _vin
             , price           = _price
             , previousOwner   = _prevOwner
             }

writeCars :: [Car] -> String -> IO ()
writeCars cars inventoryFilename = do
  writeFile tempFilename (showCars cars)
  renameFile tempFilename inventoryFilename
  where tempFilename        = inventoryFilename ++ ".tmp"
        showCars []         = ""
        showCars (car:cars) =
          (show $ year car) ++ " " ++ make car ++ " " ++ model car ++ "\n" ++
          style car ++ "\n" ++
          color car ++ "\n" ++
          mileage car ++ "\n" ++
          (if airConditioner car then "Y" else "N") ++ "\n" ++
          vinNumber car ++ "\n" ++
          (show $ price car) ++ "\n" ++
          previousOwner car ++ "\n\n" ++
          showCars cars

handleOption :: [Car] -> Integer -> String -> IO ([Car], Bool)
handleOption cars option inventoryFilename
  | optionAddCar == option = do
      car <- promptCar
      putStrLn $ "YOUR CAR: " ++ show car
      writeCars (car:cars) inventoryFilename
      putStrLn "Car has been added to inventory"
      return ((car:cars), False)
  | optionRemoveCar == option = do
      _vinNumber <- promptUserString "Enter inventory number of the car to remove: "
      let newCars = filter (\car -> _vinNumber /= vinNumber car) cars
      writeCars newCars inventoryFilename
      return (newCars, False)
  | optionDisplayCar == option = do
      _vinNumber <- promptUserString "Enter inventory number of the car to display: "
      displayCars $ filter (\car -> _vinNumber == vinNumber car) cars
      return (cars, False)
  | optionDisplayCarsByYear == option = do
      _year <- promptUserInteger "Enter the year of the car you are searching for: "
      displayCars $ filter (\car -> year car == _year) cars
      return (cars, False)
  | optionDisplayCarsByPrice == option = do
      low  <- promptUserDouble "Enter the low end of the price range to search: "
      high <- promptUserDouble "Enter the low end of the price range to search: "
      displayCars $ filter (\car -> (low <= price car) && (price car <= high)) cars
      return (cars, False)
  | optionDisplayCarsByColor == option = do
      _color <- promptUserString "Enter the color of the car you are searching for: "
      displayCars $ filter (\car -> color car == _color) cars
      return (cars, False)
  | optionDisplayAllCars == option = do
      displayCars cars
      return (cars, False)
  | optionToJson == option = do
      putStrLn $ read $ show $ encode cars
      return (cars, False)
  | optionQuit == option = do
      putStrLn "Goodbye!"
      return (cars, True)
  | otherwise = do
      return (cars, False)


displayCars :: [Car] -> IO ()
displayCars [] = do return ()
displayCars (car:remainingCars) = do
  displayCar car
  displayCars remainingCars
  where displayCar car = putStrLn $ (show $ year car) ++ ", " ++ make car ++ ", " ++ model car ++ "\n" ++
                                    "style: " ++ style car ++ "\n" ++
                                    "color: " ++ color car ++ "\n" ++
                                    "miles: " ++ mileage car ++ "\n" ++
                                    "air conditioning: " ++ (if airConditioner car then "Y" else "N") ++ "\n" ++
                                    "inventory number: " ++ vinNumber car ++ "\n" ++
                                    "price: " ++ (show $ price car) ++ "\n" ++
                                    "previous owner: " ++ previousOwner car ++ "\n" ++
                                    "________________________________\n\n"

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

menuLoop :: [Car] -> String -> IO ()
menuLoop cars inventoryFilename = do
  printMenu
  option <- getOption
  (newCars, shouldQuit) <- handleOption cars option inventoryFilename
  if shouldQuit
    then return ()
    else menuLoop newCars inventoryFilename

main :: IO ()
main = do
  cars <- readCars inventoryFilename
  menuLoop cars inventoryFilename
