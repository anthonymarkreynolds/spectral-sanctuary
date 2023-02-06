module Main (main) where

import System.IO
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import Control.Monad.State
import Menu
-- import Room

welcomeMessage :: Message
welcomeMessage = Info "Welcome to Spectral Sanctuary"

invalidInputMessage :: Message
invalidInputMessage = Alert "Invalid input, try again."

quit :: IO ()
quit = do
  _ <- clearConsole
  putStrLn "Goodbye!"
  exitSuccess

mainMenu :: Menu
mainMenu = Menu "Main" 
  [ MenuItem "New Game" Nothing 
  , MenuItem "Help" (Just helpMenu)
  , MenuItem "Quit" Nothing ]

helpMenu :: Menu
helpMenu = Menu "Help" 
  [ MenuItem "Main Menu" (Just mainMenu)
  , MenuItem "Quit" Nothing]

getChar' :: IO (Maybe Int)
getChar' = do
  hSetEcho stdin False
  char <-  getChar :: IO Char
  hSetEcho stdin True
  return $ readMaybe [char]

loop :: MenuLoop
loop = do

  gameState <- get
  let menu = currentMenu gameState
  _ <- lift $ printMenu menu (message gameState)

  -- lift roomTest

  -- Clear message 
  put $ gameState {message = Nothing}
  gameState <- get

  selected <- lift getChar'
  let itemsCount = length (items menu)
  case selected of
    Nothing -> do
      (put $ gameState {message = Just invalidInputMessage}) >> loop
    -- Check if in range of menu items
    Just n | n < 1 || n > itemsCount -> do
      (put $ gameState {message = Just invalidInputMessage}) >> loop
    Just n -> do
      let selectedItem = items menu !! (n - 1)
      case subMenu selectedItem of
        Just subMenu' -> (put $ gameState {currentMenu = subMenu'}) >> loop 
        Nothing       -> (put $ gameState {message = Just (Info $ "No submenu for: " ++ itemName selectedItem)}) >> loop

main :: IO ()
main = do
  putStrLn ""
  evalStateT loop (GameState (Just welcomeMessage) mainMenu)