module Main where

import System.Environment
import Game

main :: IO ()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO()
parseArgs [] = startGame
parseArgs y@(x:_) = do
  case x of
    "--help" -> usage
    "--version" -> version
    _ -> startGame

usage :: IO()
usage = putStrLn usageStr

usageStr :: String
usageStr = "Это обычная текстовая RPG.В этой игре\n" ++
           "вы сражаетесь с необычными врагами, которые захватили\n" ++
           "соседнюю деревню. С каждой новой победой\n" ++
           "враги будут все сильнее. Ваша задача: -\n" ++
           "освободить деревню от врагов\n"

version :: IO()
version = putStrLn versionStr

versionStr :: String
versionStr = "Text RPG.\n" ++
             "Version 4.2.2.1\n" ++
             "(c) Alexander Chekodanov, 2018"
