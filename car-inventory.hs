import System.IO (hFlush, stdout)

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
  putStrLn "1  -  Add a new car to inventory."
  putStrLn ""
  putStrLn "2  -  Remove a sold car from inventory."
  putStrLn ""
  putStrLn "3  -  Display info on car. (inv # needed)"
  putStrLn ""
  putStrLn "4  -  Display cars of specific Year."
  putStrLn ""
  putStrLn "5  -  Display cars within a price range."
  putStrLn ""
  putStrLn "6  -  Display cars of specified color."
  putStrLn ""
  putStrLn "7  -  Display All Cars in inventory."
  putStrLn ""
  putStrLn ""
  putStrLn "8  -  Quit. (exit inventory tracking system)"
  putStrLn "______________________________________________"

getOption :: IO Integer
getOption = do
  putStr "Enter your choice (1-8) ===> "
  hFlush stdout
  option <- getLine
  return $ read option

handleOption :: Integer -> IO Bool
handleOption option
  | option == 1 = do
    putStrLn "ADDING A NEW CAR"
    return False
  | option == 2 = do return False
  | option == 3 = do return False
  | option == 4 = do return False
  | option == 5 = do return False
  | option == 6 = do return False
  | option == 7 = do return False
  | option == 8 = do return True
  | otherwise   = do return False

main :: IO ()
main = do
  printMenu
  option     <- getOption
  shouldQuit <- handleOption option
  if not shouldQuit
    then main
    else return ()
