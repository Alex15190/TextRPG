module Level(
  startLevel
)where

import Player
import Enemy
import Village

startLevel :: Player -> IO ()
startLevel  player = do
  enemy <- askEnemy $ getPlayerLvl player
  let enemyDamage = getEnemyDamage player enemy
  putStrLn $ "Вы встретили: " ++ show enemy
  putStrLn $ "Ваши действия:\n" ++ "1) Ударить!\n" ++ "2) Убежать\n" ++ "3) Выйти из игры"
  sChoose <- getLine
  let iChoose = read sChoose :: Int
  case iChoose of
    1 -> nextFight player{pHealth = (pHealth player - enemyDamage)} enemy{eHealth = (eHealth enemy - d)}
    2 -> do
      player <- goToVillage player
      startLevel player
    3 -> exitGame
    _ -> exitGame
  where
    d = getDamage $ player


nextFight :: Player -> Enemy -> IO ()
nextFight player enemy = do
  if (isPlayerDead player) then
    gameOver
  else do
    if (isEnemyDead enemy) then do
      putStrLn "Вы одолели врага!"
      if (pLvl player == 10) then
        goodGameOver
      else do
        player <- goToVillage player{pLvl = pLvl player + 1, pGold = pGold player + pLvl player }
        startLevel player
    else
      fight player enemy

fight :: Player -> Enemy -> IO ()
fight player enemy = do
  putStrLn $ "Ваше здоровье: " ++ show (pHealth player) ++ " Здоровье врага: " ++ show (eHealth enemy)
  putStrLn $ "Ваши действия:\n" ++ "1) Ударить!\n" ++ "2) Убежать\n" ++ "3) Выйти из игры"
  sChoose <- getLine
  let iChoose = read sChoose :: Int
  case iChoose of
      1 -> nextFight player{pHealth = (pHealth player - enemyDamage)} enemy{eHealth = (eHealth enemy - d)}
      2 -> do
        player <- goToVillage player
        startLevel player
      3 -> exitGame
      _ -> exitGame
    where
      d = getDamage $ player
      enemyDamage = getEnemyDamage player enemy

gameOver :: IO()
gameOver = do
  putStrLn "Вы проиграли!"

goodGameOver :: IO()
goodGameOver = do
  putStrLn "Вы победили!"

exitGame :: IO()
exitGame  = do
  putStrLn "Спасибо за игру =)"
