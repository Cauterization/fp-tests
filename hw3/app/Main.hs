module Main
  ( main
  ) where

import Control.Monad.IO.Class
import System.Console.Haskeline
import Data.Set ( empty, fromList )

import HW3.Action
import HW3.Base 
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty


main :: IO ()
main = do
    putStrLn "\n\n\n\n\n"
    runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
    minput <- getInputLine "% "
    case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just ":q"   -> return ()
        Just "q"    -> return ()
        Just input  -> process input
  
process :: String -> InputT IO ()
process inp = case parse inp of
    Left err -> do
        outputStrLn $ show err
        loop
    Right r  -> do
        outputStrLn "\nPARSED:\n"
        outputStrLn $ show r
        outputStrLn "\nEVALUATED:\n"
        x <- liftIO ((eval r)) 
        outputStrLn $ show x
        outputStrLn "\nPPRINTED:\n"
        outputStrLn $ show $ either showDoc prettyValue x
        outputStrLn "\n\n\n\n\n"
        loop

dummyRun :: HIO a -> IO a
dummyRun m = runHIO m empty

permRun :: HIO a -> IO a
permRun m = runHIO m $ fromList [AllowRead, AllowWrite]

instance HiMonad IO where
    runAction a = (`runHIO` (fromList [AllowRead, AllowWrite])) $ runAction a