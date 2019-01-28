module Level(
  startLevel
)where

import Player
import Enemy
import Village

startLevel :: Player -> IO ()
startLevel  player = do
  putStrLn $ "Вы встретили: " ++ show enemy
  putStrLn "Ваши действия: 1) Убежать 2) Ударить!"
  sChoose <- getLine
  let iChoose = read sChoose :: Int
  case iChoose of
       1 -> gameOver
       2 -> nextFight player{pHealth = (pHealth player - enemyDamage)} enemy{eHealth = (eHealth enemy - d)}
  where
    enemy = askEnemy $ getPlayerLvl player
    d = getDamage $ player
    enemyDamage = getEnemyDamage player enemy

nextFight :: Player -> Enemy -> IO ()
nextFight player enemy = do
  if (isPlayerDead player) then
    gameOver
  else do
    if (isEnemyDead enemy) then
      goToVillage player
      startLevel newPlayer
    else
      fight player enemy

fight :: Player -> Enemy -> IO ()
fight player enemy = do
  putStrLn $ "Ваше здоровье: " ++ show (pHealth player) ++ " Здоровье врага: " ++ show (eHealth enemy)
  putStrLn "Ваши действия: 1) Убежать 2) Ударить!"
  sChoose <- getLine
  let iChoose = read sChoose :: Int
  case iChoose of
      1 -> gameOver
      2 -> nextFight player{pHealth = (pHealth player - enemyDamage)} enemy{eHealth = (eHealth enemy - d)}
    where
      d = getDamage $ player
      enemyDamage = getEnemyDamage player enemy

gameOver :: IO()
gameOver = do
  putStrLn "Вы проиграли!"

goodGameOver :: IO()
goodGameOver = do
  putStrLn "Вы победили!"
