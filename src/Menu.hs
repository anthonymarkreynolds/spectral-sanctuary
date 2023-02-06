module Menu where

import Control.Monad.State
import System.Process 
import GHC.IO.Exception (ExitCode)

data Message = Info String | Alert String | Plain String deriving (Show)

data GameState = GameState 
  { message     :: Maybe Message
  , currentMenu :: Menu }

type MenuLoop = StateT GameState IO ()

type Action = IO ()

data MenuItem = MenuItem 
  { itemName :: String
  , subMenu  :: Maybe Menu }

data Menu = Menu {menuName :: String, items :: [MenuItem]} 

clearConsole :: IO ExitCode
clearConsole = system "clear"

printMessage :: Maybe Message -> IO ()
printMessage (Just message) = case message of
  Info  str -> putStrLn $ "🛈 " ++ str
  Alert str -> putStrLn $ "⚠ " ++ str
  Plain str -> putStrLn str
printMessage Nothing = return ()

printMenu :: Menu -> Maybe Message -> IO ()
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
