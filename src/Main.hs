module Main (main) where

import System.Process 
import GHC.IO.Exception (ExitCode)
import System.IO
import Text.Read (readMaybe)
import Control.Monad.State

clearConsole :: IO ExitCode
clearConsole = system "clear"

welcomeMessage :: String
welcomeMessage = "Welcome to Spectral Sanctuary"

invalidInputMessage :: String
invalidInputMessage = "Invalid input, try again."

-- data Message = Info | Alert
data GameState = GameState {message :: Maybe String}

type MenuLoop = Menu -> StateT GameState IO ()

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

printMessage :: Maybe String -> IO ()
printMessage (Just message) = putStrLn message
printMessage Nothing = return ()

printMenu :: Menu -> Maybe String -> IO ()
printMenu (Menu {menuName = menuName, items = items}) message = do
  _ <- clearConsole
  printMessage message
  putStrLn $ "Current Menu - " ++ menuName
  putStrLn "--------------"
  putStrLn "Press a number to select a menu item:"
  mapM_ printWithIndex $ zip [1..] (itemName <$> items)
    where
      printWithIndex :: (Int, String) -> IO ()
      printWithIndex (i, x) = putStrLn (show i ++ ". " ++ x)

main :: IO ()
main = do
  putStrLn ""
  evalStateT (loop mainMenu) (GameState (Just welcomeMessage))

getChar' :: IO (Maybe Int)
getChar' = do
  hSetEcho stdin False
  char <-  getChar :: IO Char
  hSetEcho stdin True
  return $ readMaybe [char]


loop :: MenuLoop
loop menu = do
  gameState <- get
  _ <- lift $ printMenu menu (message gameState)
  put $ gameState {message = Nothing}
  selected <- lift getChar'
  let itemsCount = length (items menu)
  case selected of
    Nothing -> do
      put $ gameState {message = Just invalidInputMessage}
      loop menu 
    -- Check if in range of menu items
    Just n | n < 1 || n > itemsCount -> do
      put $ gameState {message = Just invalidInputMessage}
      loop menu 
    Just n -> do
      let selectedItem = items menu !! (n - 1)
      case subMenu selectedItem of
        Just subMenu' -> loop subMenu'
        Nothing       -> loop mainMenu

-- newGameLoop :: IO ()
-- newGameLoop = do
--   putStrLn "Starting a new game..."
--   -- Do any necessary setup for the new game here
--   loop mainMenu

-- helpLoop :: IO ()
-- helpLoop = do
--   putStrLn "Displaying help information..."
--   -- Do any necessary setup for the help menu here
--   loop helpMenu

-- quitProgram :: Menu
-- quitProgram = do putStrLn "Goodbye!"

--   putStrLn "Press a key"
--   hSetEcho stdin False
--   char <-  getChar
--   hSetEcho stdin True
--   let charStr = [char]
--   case readMaybe charStr of
--     Just c -> putStrLn ("You pressed: " ++ c)
--     Nothing -> putStrLn  $ "Invalid Input: " ++ [char]
--   main