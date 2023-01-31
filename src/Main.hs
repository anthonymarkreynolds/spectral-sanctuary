module Main (main) where

import System.Process 
import GHC.IO.Exception (ExitCode)

clearConsole :: IO ExitCode
clearConsole = system "clear"

welcomeMessage :: IO ()
welcomeMessage = putStrLn "Welcome to Spectral Sanctuary"

type MenuLoop = Menu -> IO ()

data MenuItem = MenuItem {itemName :: String, subMenu:: Maybe Menu} 

data Menu = Menu {menuName :: String, items :: [MenuItem]} 

mainMenu :: Menu
mainMenu = Menu "Main" 
  [ MenuItem "New Game" Nothing
  , MenuItem "Help" (Just helpMenu)
  , MenuItem "Quit" Nothing ]

helpMenu :: Menu
helpMenu = Menu "Help" 
  [ MenuItem "Main Menu" (Just mainMenu) 
  , MenuItem "Quit" Nothing]

printMenu :: Menu -> IO ()
printMenu (Menu {menuName = menuName, items = items}) = do
  putStrLn $ "Current Menu - " ++ menuName
  putStrLn "--------------"
  mapM_ printWithIndex $ zip [1..] (itemName <$> items)
    where
      printWithIndex :: (Int, String) -> IO ()
      printWithIndex (i, x) = putStrLn (show i ++ ". " ++ x)

main :: IO ()
main = do
  welcomeMessage
  putStrLn ""
  loop mainMenu

loop :: MenuLoop
loop menu = do
  printMenu menu
  putStrLn "Enter a number to select a menu item:"
  selected <- readLn :: IO Int
  let itemsCount = length (items menu)
  case selected of
    -- Check for invalid input
    n | n < 1 || n > itemsCount -> do
      putStrLn "Invalid input, try again."
      loop menu
    n -> do
      let selectedItem = items menu !! (n - 1)
      case subMenu selectedItem of
        Just subMenu' -> loop subMenu'
        Nothing       -> loop mainMenu

      -- case itemName selectedItem of
      --   -- Add cases for each menu item
      --   "New Game" -> newGameLoop
      --   "Help" -> helpLoop
      --   "Quit" -> quitProgram

newGameLoop :: IO ()
newGameLoop = do
  putStrLn "Starting a new game..."
  -- Do any necessary setup for the new game here
  loop mainMenu

helpLoop :: IO ()
helpLoop = do
  putStrLn "Displaying help information..."
  -- Do any necessary setup for the help menu here
  loop helpMenu

quitProgram :: IO ()
quitProgram = putStrLn "Goodbye!"

--   putStrLn "Press a key"
--   hSetEcho stdin False
--   char <-  getChar
--   hSetEcho stdin True
--   let charStr = [char]
--   case readMaybe charStr of
--     Just c -> putStrLn ("You pressed: " ++ c)
--     Nothing -> putStrLn  $ "Invalid Input: " ++ [char]
--   main