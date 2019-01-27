module Village(
  goToVillage
)where

import Player
import Level

goToVillage :: Player -> IO ()
goToVillage p = do
  putStrLn "Вы в деревне. Вы можете тут поправить свое здоровье и взять новое оружие и броню, если вы убили достаточно врагов."
  inVillage p

inVillage :: Player -> IO ()
inVillage p = do
  putStrLn $ "Ваши действия:" ++
             "1) Посетить больницу" ++
             "2) Посетить кузнеца" ++
             "3) Посетить Мэра" ++
             "4) Пойти к врагам"
  ch <- getLine
  number read ch :: Int
  case number of
       1 -> goToHospital
       2 -> goToBlacksmith --Кузнец
       3 -> goToMayor --Мэр
       4 -> goToEnemy
       _ -> goToEnemy


goToHospital :: Player -> IO ()
goToHospital p = do
  putStrLn "Добро пожаловать в больницу. Тут вы можете поправить свое здоровье"
  if (isEnouthGold p 3) then
    putStrLn $ "Вылечить вас за 3 золотых?" ++
              "1) Да" ++
              "2) Нет"
    ch <- getLine
    number read ch :: Int
    case number of
        1 -> inVillage p{ pHealth = 10, pGold = pGold p - 3}
        2 -> inVillage p
        _ -> inVillage p
  else
    putStrLn "У вас недостаточно денег!"
    inVillage p

goToBlacksmith :: Player -> IO ()
goToBlacksmith p = do



goToMayor :: Player -> IO ()
goToMayor p = do



goToEnemy :: Player -> IO ()
goToEnemy p = do
