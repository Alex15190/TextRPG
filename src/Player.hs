module Player(
  WeaponType(..),
  HelmetArmor(..),
  BodyArmor(..),
  BootsArmor(..),
  Armor(..),
  Weapon(..),
  Player(..),
  askName,
  chooseWeapon,
  askWeapon,
  askHelmetArmor,
  askBodyArmor,
  askBootsArmor,
  askArmor,
  createArmor,
  createWeapon,
  createCharacter,
  getDamage,
  getArmorDefence,
  isPlayerDead
)where

data WeaponType = Sword | Bow | Hammer | Knife deriving (Eq, Ord, Enum)
data HelmetArmor = HelmetArmor{
                helmetName :: String,
                helmetPoints :: Int
              }
              
data BodyArmor = BodyArmor{
                  bodyName :: String,
                  bodyPoints :: Int
              }
              
data BootsArmor = BootsArmor{
                    bootsName :: String,
                    bootsPoints :: Int
              }


data Armor = Armor{
                armorHelmet :: HelmetArmor,
                armorBody :: BodyArmor,
                armorBoots :: BootsArmor
              }
              
data Weapon = Weapon{
                wType :: WeaponType,
                wDamage :: Int
              }
              
data Player = Player{
                pName :: String,
                pHealth :: Int,
                pWeapon :: Weapon,
                pArmor :: Armor
              }

instance Show WeaponType where 
  show wt = 
    case wt of
      Sword  -> "меч"
      Bow    -> "лук"
      Hammer -> "молот"
      Knife  -> "нож"
      
      
instance Show HelmetArmor where
  show ha = concat [helmetName $ ha, 
                    " с защитой = ",
                    show . helmetPoints $ ha] 

instance Show BodyArmor where
  show ba = concat [bodyName $ ba, 
                    " с защитой = ",
                    show . bodyPoints $ ba] 
                    
instance Show BootsArmor where
  show boa = concat [bootsName $ boa, 
                    " с защитой = ",
                    show . bootsPoints $ boa] 
                    
instance Show Armor where
  show a = concat[show . armorHelmet $ a,
                  ", ",
                  show . armorBody $ a,
                  ", ",
                  show . armorBoots $ a]
      
instance Show Weapon where
  show w = concat [show . wType $ w, 
                   ", урон = ", 
                   show . wDamage $ w]
      
askName :: IO String
askName = do
  putStrLn "Введите свое имя: "
  name <- getLine
  return name
  
chooseWeapon :: Int -> WeaponType
chooseWeapon i =
  case i of
    1 -> Sword
    2 -> Bow
    3 -> Hammer
    4 -> Knife
    _ -> Sword
  
askWeapon :: IO Weapon
askWeapon = do
  putStrLn $ "Выберите оружие: \n" ++ 
             "1) Меч\n"            ++ 
             "2) Лук\n"            ++ 
             "3) Молот\n"          ++ 
             "4) Нож\n"
  i <- getLine
  let index = read i :: Int
  return $ Weapon {wType = chooseWeapon index, wDamage = 3}
  
askHelmetArmor :: HelmetArmor
askHelmetArmor = do
  HelmetArmor{helmetName = "Шлем отшельника", helmetPoints = 1}
  
askBodyArmor :: BodyArmor
askBodyArmor = do
  BodyArmor{bodyName = "нагрудник отшельника", bodyPoints = 1}
  
askBootsArmor :: BootsArmor
askBootsArmor = do
  BootsArmor{bootsName = "ботинки отшельника", bootsPoints = 1}

askArmor :: Armor
askArmor = do
  Armor{armorHelmet = helmet, armorBody = body, armorBoots = boots}
   where
     helmet = askHelmetArmor
     body = askBodyArmor
     boots = askBootsArmor
  
createArmor :: HelmetArmor -> BodyArmor -> BootsArmor -> Armor
createArmor h b bo =
  Armor{armorHelmet = h, armorBody = b, armorBoots = bo}
  

              
createWeapon :: WeaponType -> Int -> Weapon
createWeapon w d =
  Weapon{wType = w, wDamage = d}
              
createCharacter :: String -> Int -> Weapon -> Armor -> Player
createCharacter n h w a =
  Player{pName = n, pHealth = h, pWeapon = w, pArmor = a}
  
getDamage :: Player -> Int
getDamage p = 
  wDamage $ pWeapon p
  
getArmorDefence :: Player -> Int
getArmorDefence p = bop + bp + hp
  where
    a   = pArmor p
    bo  = armorBoots a
    b   = armorBody a
    h   = armorHelmet a
    bop = bootsPoints bo
    bp  = bodyPoints b
    hp  = helmetPoints h

isPlayerDead :: Player -> Bool
isPlayerDead p = 
  if (pHealth p > 0) then
    False
  else
    True
