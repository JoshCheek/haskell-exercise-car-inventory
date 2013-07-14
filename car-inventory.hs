import System.IO (hFlush, stdout)

optionAddCar             = 1 :: Integer
optionRemoveCar          = 2 :: Integer
optionDisplayCar         = 3 :: Integer
optionDisplayCarsByYear  = 4 :: Integer
optionDisplayCarsByPrice = 5 :: Integer
optionDisplayCarsByColor = 6 :: Integer
optionDisplayAllCars     = 7 :: Integer
optionQuit               = 8 :: Integer

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

getOption :: IO Integer
getOption = do
  putStr "Enter your choice (1-8) ===> "
  hFlush stdout
  option <- getLine
  return $ read option

handleOption :: Integer -> IO Bool
handleOption option
  | option == optionAddCar = do
      putStrLn "ADD A NEW CAR"
      return False
  | option == optionRemoveCar = do
      putStrLn "REMOVE CAR"
      return False
  | option == optionDisplayCar = do
      putStrLn "DISPLAY CAR"
      return False
  | option == optionDisplayCarsByYear = do
      putStrLn "DISPLAY CAS BY YEAR"
      return False
  | option == optionDisplayCarsByPrice = do
      putStrLn "DISPLAY CARS BY PRICE"
      return False
  | option == optionDisplayCarsByColor = do
      putStrLn "DISPLAY CARS BY COLOUR"
      return False
  | option == optionDisplayAllCars = do
      putStrLn "DISPLAY ALL CARS"
      return False
  | option == optionQuit = do
      putStrLn "QUITTING"
      return True

main :: IO ()
main = do
  printMenu
  option     <- getOption
  shouldQuit <- handleOption option
  if not shouldQuit
    then main
    else return ()
