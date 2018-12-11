module Level(
  startLevel
)where

import Player
import Enemy
  
startLevel :: Int -> Player -> IO ()
startLevel index player = do
  putStrLn $ "Вы встретили: " ++ show enemy
  putStrLn "Ваши действия: 1) Убежать 2) Ударить!"
  sChoose <- getLine
  let iChoose = read sChoose :: Int 
  case iChoose of
       1 -> gameOver
       2 -> nextFight index player enemy{eHealth = (eHealth enemy - d)}
  where
    enemy = askEnemy index
    d = getDamage $ player
       
nextFight :: Int -> Player -> Enemy -> IO ()
nextFight index player enemy = do
  if (isPlayerDead player) then
    gameOver
  else do
    if (isEnemyDead enemy) then
      goodGameOver
    else 
      fight index player enemy
    
fight :: Int -> Player -> Enemy -> IO ()
fight index player enemy = do
  putStrLn $ "Ваше здоровье: " ++ show (pHealth player) ++ " Здоровье врага: " ++ show (eHealth enemy)
  putStrLn "Ваши действия: 1) Убежать 2) Ударить!"
  sChoose <- getLine
  let iChoose = read sChoose :: Int 
  case iChoose of
      1 -> gameOver
      2 -> nextFight index player enemy{eHealth = (eHealth enemy - d)}
    where
      d = getDamage $ player
    
gameOver :: IO()
gameOver = do
  putStrLn "Вы проиграли!"

goodGameOver :: IO()
goodGameOver = do
  putStrLn "Вы победили!"
