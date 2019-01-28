module Village(
  goToVillage
)where

import Player

goToVillage :: Player -> IO Player
goToVillage p = do
  putStrLn $ "Вы в деревне. Вы можете тут поправить свое здоровье и взять " ++
             "новое оружие и броню, если вы убили достаточно врагов."
  inVillage p

inVillage :: Player -> IO Player
inVillage p = do
  putStrLn $ "Ваши действия:" ++
             "1) Посетить больницу" ++
             "2) Посетить кузнеца" ++
             "3) Посетить Мэра" ++
             "4) Пойти к врагам"
  ch <- getLine
  let number = read ch :: Int
  case number of
       1 -> goToHospital p   --Больница
       2 -> goToBlacksmith p --Кузнец
       3 -> goToMayor p      --Мэр
       4 -> goToEnemy p      --Враги
       _ -> goToEnemy p

goToHospital :: Player -> IO Player
goToHospital p = do
  putStrLn "Добро пожаловать в больницу. Тут вы можете поправить свое здоровье"
  if (isEnoughGold p 3) then do
    putStrLn $ "Вылечить вас за 3 золотых?" ++
              "1) Да" ++
              "2) Нет"
    ch <- getLine
    let number = read ch :: Int
    case number of
        1 -> inVillage p{ pHealth = 10, pGold = pGold p - 3}
        2 -> inVillage p
        _ -> inVillage p
  else do
    putStrLn "У вас недостаточно денег!"
    inVillage p

goToBlacksmith :: Player -> IO Player
goToBlacksmith p = do
    putStrLn $ "Приветствую тебя " ++ pName p ++ ". Я кузнец Алвор из Ривервуда." ++
               "Тут ты можешь прикупить себе оружие и броню, если золота" ++
               "хватит."
    putStrLn $ show helmet ++
               show body ++
               show boots ++
               show weapon
    inVillage p

  where
    l = pLvl p
    helmet = HelmetArmor{helmetName = "Имперский шлем", helmetPoints = l + 1}
    body = BodyArmor{bodyName = "Сыромятная броня", bodyPoints = l - 1}
    boots = BootsArmor{bootsName = "Сапоги изгоев", bootsPoints = l}
    weapon = Weapon{wType = Sword, wDamage = l + 1 }




goToMayor :: Player -> IO Player
goToMayor p = do
  putStrLn $ "Здравствуй " ++ pName p ++
             "Я Мэр этой деревни, как бы странно это не звучало." ++
             "Тут ты можешь передохнуть и востановить все свои " ++
             "силы перед следующим боем. "
  inVillage p

goToEnemy :: Player -> IO Player
goToEnemy p = do
  putStrLn "Вы направляетесь к врагам. Не теряйте бдительности. Рано или поздно она окупится."
  return p
