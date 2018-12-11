module Game(
  startGame
)where

import Level 
import Player 

    
  
startGame :: IO()
startGame = do
  n <- askName
  w <- askWeapon
  runGame $ createCharacter n 10 w a
  where
    a = askArmor

runGame :: Player -> IO()
runGame p = do
  putStrLn $ concat ["Имя: ",       pName p,
                     "\nЗдоровье ", show . pHealth $ p, 
                     "\nОружие: ", show . pWeapon $ p]
  startLevel 0 p
