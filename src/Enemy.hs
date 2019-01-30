module Enemy(
  EnemyType(..),
  Enemy(..),
  askEnemy,
  isEnemyDead,
  getEnemyDamage
)where

import Player
import System.Random

data EnemyType =  Orc | HalfOgre | Elf | Zombie deriving (Eq, Ord, Enum)

data Enemy = Enemy{
                eType :: EnemyType,
                eDamage :: Int,
                eHealth :: Int
              }

instance Show EnemyType where
  show et =
    case et of
      Orc      -> "орк"
      HalfOgre -> "полуОгр"
      Elf      -> "эльф"
      Zombie   -> "зомби"

instance Show Enemy where
  show e = concat [show . eType $ e,
                   " с уроном: ",
                   show . eDamage $ e,
                   " и жизнями: ",
                   show . eHealth $ e]

askEnemy :: Int -> IO Enemy
askEnemy i = do
  gi <- randomRIO (1::Int, 4::Int)
  case gi of
    1 -> return Enemy{eType = Orc, eDamage = 2 + i, eHealth = 4 + i}
    2 -> return Enemy{eType = HalfOgre, eDamage = 2 + i, eHealth = 5 + i}
    3 -> return Enemy{eType = Elf, eDamage = 3 + i, eHealth = 3 + i}
    4 -> return Enemy{eType = Zombie, eDamage = 1 + i, eHealth = 6 + i}
    _ -> return Enemy{eType = Orc, eDamage = 2 + i, eHealth = 4 + i}


isEnemyDead :: Enemy -> Bool
isEnemyDead e =
  if (eHealth e > 0) then
    False
  else
    True

getEnemyDamage :: Player -> Enemy -> Int
getEnemyDamage p e =
  if (getArmorDefence p >= eDamage e) then
    0
  else
    eDamage e - (getArmorDefence p)
