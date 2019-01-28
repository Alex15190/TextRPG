module Game(
  startGame
)where

import Level
import Player



startGame :: IO()
startGame = do
  n <- askName
  w <- askWeapon
  runGame $ createCharacter n 10 w a 10 1
  where
    a = askArmor

runGame :: Player -> IO()
runGame p = do
  playerInfo p
  startLevel p
