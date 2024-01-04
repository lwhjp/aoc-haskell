import Data.Ix
import Data.List
import Data.List.Split
import Text.Regex.PCRE

readEntry :: String -> [(String, String)]
readEntry = map readPair . words
  where
    readPair = fmap tail . break (== ':')

validators :: [(String, String -> Bool)]
validators =
  [ ("byr", yearBetween (1920, 2002)),
    ("iyr", yearBetween (2010, 2020)),
    ("eyr", yearBetween (2020, 2030)),
    ("hgt", maybe False validHeight . (=~~ "^(\\d+)(cm|in)$")),
    ("hcl", (=~ "^#[0-9a-f]{6}$")),
    ("ecl", (=~ "^(amb|blu|brn|gry|grn|hzl|oth)$")),
    ("pid", (=~ "^\\d{9}$"))
  ]
  where
    yearBetween :: (Int, Int) -> String -> Bool
    yearBetween r = maybe False (inRange r . read) . (=~~ "^\\d{4}$")
    validHeight :: (String, String, String, [String]) -> Bool
    validHeight (_, _, _, [n, "cm"]) = inRange (150, 193) $ read n
    validHeight (_, _, _, [n, "in"]) = inRange (59, 76) $ read n

validFields, validValues :: [(String, String)] -> Bool
validFields = null . (map fst validators \\) . map fst
validValues ps =
  and
    [ maybe False pred $ lookup field ps
      | (field, pred) <- validators
    ]

main = do
  input <- map readEntry . splitOn "\n\n" <$> readFile "input04"
  print $ length . filter validFields $ input
  print $ length . filter validValues $ input
