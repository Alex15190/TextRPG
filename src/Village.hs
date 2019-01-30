module Village(
  goToVillage
)where

import Player
import System.Random

goToVillage :: Player -> IO Player
goToVillage p = do
  putStrLn $ "Вы в деревне. Вы можете тут поправить свое здоровье и купить\n" ++
             "новое оружие и броню, если у вас достаточно золота."
  inVillage p

inVillage :: Player -> IO Player
inVillage p = do
  putStrLn $ "Ваши действия:\n" ++
             "1) Посетить больницу\n" ++
             "2) Посетить кузнеца\n" ++
             "3) Посетить Мэра\n" ++
             "4) Посмотреть свои характеристики\n" ++
             "5) Пойти к врагам\n"
  ch <- getLine
  let number = read ch :: Int
  case number of
       1 -> goToHospital p   --Больница
       2 -> goToBlacksmith p --Кузнец
       3 -> goToMayor p      --Мэр
       4 -> do               --Информация об игроке
         playerInfo p
         inVillage p
       5 -> goToEnemy p      --Враги
       _ -> goToEnemy p

goToHospital :: Player -> IO Player
goToHospital p = do
  putStrLn "Добро пожаловать в больницу. Тут вы можете поправить свое здоровье"
  if (isEnoughGold p 3) then do
    putStrLn $ "Вылечить вас за 3 золотых?\n" ++
              "1) Да\n" ++
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
    putStrLn $ "Приветствую тебя " ++ pName p ++ ". Я кузнец Алвор из Ривервуда.\n" ++
               "Тут ты можешь прикупить себе оружие и броню, если золота\n" ++
               "хватит."
    weapon <- askRandomWeapon l
    putStrLn $ "1) " ++ show helmet ++ " за " ++ price ++ " золотых,\n" ++
               "2) " ++ show body ++ " за " ++ price ++ " золотых,\n" ++
               "3) " ++ show boots ++ " за " ++ price ++ " золотых,\n" ++
               "4) " ++ show weapon ++ " за " ++ price ++ " золотых,\n" ++
               "5) Ничего не покупать"
    ch <- getLine
    let number = read ch :: Int
    case number of
        1 -> do
          if (isEnoughGold p (l * 2)) then
            inVillage p{ pArmor = playerArmor{ armorHelmet = helmet}, pGold = pGold p - l - l}
          else do
            putStrLn "У вас недостаточно золота!\n"
            inVillage p
        2 -> do
          if (isEnoughGold p (l * 2)) then
            inVillage p{ pArmor = playerArmor{ armorBody = body}, pGold = pGold p - l - l}
          else do
            putStrLn "У вас недостаточно золота!\n"
            inVillage p
        3 -> do
          if (isEnoughGold p (l * 2)) then
            inVillage p{ pArmor = playerArmor{ armorBoots = boots}, pGold = pGold p - l - l}
          else do
            putStrLn "У вас недостаточно золота!\n"
            inVillage p
        4 -> do
          if (isEnoughGold p (l * 2)) then
            inVillage p{ pWeapon = weapon, pGold = pGold p - l - l}
          else do
            putStrLn "У вас недостаточно золота!\n"
            inVillage p
        5 -> inVillage p
        _ -> inVillage p
  where
    l = pLvl p
    price = show $ l * 2
    helmet = HelmetArmor{helmetName = "Имперский шлем", helmetPoints = l + 1}
    body = BodyArmor{bodyName = "Сыромятная броня", bodyPoints = l - 1}
    boots = BootsArmor{bootsName = "Сапоги эльфов", bootsPoints = l}
    playerArmor = pArmor p




goToMayor :: Player -> IO Player
goToMayor p = do
  putStrLn $ "Здравствуй " ++ pName p ++ "\n" ++
             "Я Мэр этой деревни, как бы странно это не звучало.\n" ++
             "Тут ты можешь передохнуть и востановить все свои\n" ++
             "силы перед следующим боем.\n"
  inVillage p

goToEnemy :: Player -> IO Player
goToEnemy p = do
  putStrLn "Вы направляетесь к врагам. Не теряйте бдительности. Рано или поздно она окупится."
  return p

askRandomWeapon :: Int -> IO Weapon
askRandomWeapon l = do
  gi <- randomRIO (1::Int, 4::Int)
  case gi of
    1 -> return Weapon{wType = Sword,  wDamage = l + 2 }
    2 -> return Weapon{wType = Bow,    wDamage = l  }
    3 -> return Weapon{wType = Hammer, wDamage = l + 3 }
    4 -> return Weapon{wType = Knife,  wDamage = l + 1 }
    _ -> return Weapon{wType = Sword,  wDamage = l + 2 }
