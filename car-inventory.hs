import System.IO (hFlush, stdout, readFile, writeFile)
import System.Directory (renameFile)

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
             show optionDisplayAllCars     ++ "  -  Display All Cars in inventory.\n\n\n"       ++
             show optionQuit               ++ "  -  Quit. (exit inventory tracking system)\n\n" ++
             "______________________________________________"

promptUser :: String -> IO String
promptUser message = do
  putStr message
  hFlush stdout
  getLine

getOption :: IO Integer
getOption = do
  option <- promptUser optionPrompt
  if elem (read option) options
    then return (read option)
    else getOption

promptCar :: IO Car
promptCar = do
  putStrLn $  "Input data for new car:\n" ++
              "________________________________\n\n"
  _year      <- promptUser "Input year: "
  _make      <- promptUser "Input make: "
  _model     <- promptUser "Input model: "
  _style     <- promptUser "Input style: "
  _color     <- promptUser "Input color: "
  _miles     <- promptUser "Input mileage: "
  _ac        <- promptUser "Input air conditioner (y/n): "
  _vin       <- promptUser "Input inventory number: "
  _price     <- promptUser "Input price: "
  _prevOwner <- promptUser "Input previous owner: "
  return Car { year            = read _year
             , make            = _make
             , model           = _model
             , style           = _style
             , color           = _color
             , mileage         = _miles
             , airConditioner  = if _ac == "Y" || _ac == "y" then True else False
             , vinNumber       = _vin
             , price           = read _price
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
      _vinNumber <- promptUser "Enter inventory number of the car to remove: "
      let newCars = filter (\car -> _vinNumber /= vinNumber car) cars
      writeCars newCars inventoryFilename
      return (newCars, False)
  | optionDisplayCar == option = do
      _vinNumber <- promptUser "Enter inventory number of the car to display: "
      displayCars $ filter (\car -> _vinNumber == vinNumber car) cars
      return (cars, False)
  | optionDisplayCarsByYear == option = do
      _year <- promptUser "Enter the year of the car you are searching for: "
      displayCars $ filter (\car -> year car == read(_year)) cars
      return (cars, False)
  | optionDisplayCarsByPrice == option = do
      low  <- promptUser "Enter the low end of the price range to search: "
      high <- promptUser "Enter the low end of the price range to search: "
      displayCars $ filter (\car -> (read low <= price car) && (price car <= read high)) cars
      return (cars, False)
  | optionDisplayCarsByColor == option = do
      _color <- promptUser "Enter the color of the car you are searching for: "
      displayCars $ filter (\car -> color car == _color) cars
      return (cars, False)
  | optionDisplayAllCars == option = do
      displayCars cars
      return (cars, False)
  | optionQuit == option = do
      putStrLn "Goodbye!"
      return (cars, True)

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
