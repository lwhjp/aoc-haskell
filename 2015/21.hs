import Data.List

data Item = Item {itemCost :: Int, itemDamage :: Int, itemArmor :: Int}

readItem s = let [c, d, a] = map read $ words $ drop 12 s in Item c d a

weapons =
  readItem
    <$> [ "Dagger        8     4       0",
          "Shortsword   10     5       0",
          "Warhammer    25     6       0",
          "Longsword    40     7       0",
          "Greataxe     74     8       0"
        ]

armor =
  readItem
    <$> [ "Leather      13     0       1",
          "Chainmail    31     0       2",
          "Splintmail   53     0       3",
          "Bandedmail   75     0       4",
          "Platemail   102     0       5"
        ]

rings =
  readItem
    <$> [ "Damage +1    25     1       0",
          "Damage +2    50     2       0",
          "Damage +3   100     3       0",
          "Defense +1   20     0       1",
          "Defense +2   40     0       2",
          "Defense +3   80     0       3"
        ]

chooseUpTo n = filter ((<= n) . length) . subsequences

selections =
  concat
    <$> sequence
      [ singleton <$> weapons,
        chooseUpTo 1 armor,
        chooseUpTo 2 rings
      ]

beats (hp2, damage2, armor2) (hp1, damage1, armor1) =
  let rounds1 = (hp1 - 1) `quot` max 1 (damage2 - armor1)
      rounds2 = (hp2 - 1) `quot` max 1 (damage1 - armor2)
   in rounds1 >= rounds2

player items = (100, sum $ map itemDamage items, sum $ map itemArmor items)

selectionBeats boss = beats boss . player

selectionCost = sum . map itemCost

main = do
  let boss = (104, 8, 1)
      (winning, losing) = partition (selectionBeats boss) selections
  print $ minimum $ map selectionCost winning
  print $ maximum $ map selectionCost losing
