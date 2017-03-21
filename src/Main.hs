module Main where

import Parser

import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

processFile :: String -> IO ()
processFile fname = readFile fname >>= process

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

main :: IO ()
main = do
  args <- getArgs
  case args of
    []        -> repl
    [fname]   -> processFile fname >> return ()
