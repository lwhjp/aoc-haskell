import Data.List
import Data.List.Split
import qualified Data.Map.Lazy as Map

readInput :: String -> ([Char], [Int])
readInput s =
  let [a, b] = words s
   in (a, map read $ splitOn "," b)

arrangements :: ([Char], [Int]) -> Int
arrangements (conditions, groups) = snd $ Map.findMax searchResults
  where
    searchResults =
      Map.fromDistinctDescList
        [ ((n, j), search (n, pat) (j, gs))
          | (n, pat) <- zip (reverse [0 .. length conditions]) $ tails conditions,
            (j, gs) <- zip (reverse [0 .. length groups]) $ tails groups
        ]
    search (n, pat) (j, gs) =
      case gs of
        [] | '#' `notElem` pat -> 1
        (g : _)
          | g <= n ->
              let a =
                    if '.' `notElem` take g pat
                      && (g == n || pat !! g /= '#')
                      then searchResults Map.! (max 0 $ n - (g + 1), j - 1)
                      else 0
                  b =
                    if head pat /= '#'
                      then searchResults Map.! (n - 1, j)
                      else 0
               in a + b
        _ -> 0

expand (pat, gs) =
  (intercalate "?" $ replicate 5 pat, concat $ replicate 5 gs)

main = do
  input <- map readInput . lines <$> readFile "input12"
  print $ sum $ map arrangements input
  print $ sum $ map (arrangements . expand) input
