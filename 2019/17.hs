{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Control.Lens
import Control.Monad.State
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as Array
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as Vector
import IntCode

type Pos = (Int, Int)

data Robot = Robot {_rOutput :: [Int]}

makeLenses ''Robot

readCamera :: State Robot (Array Pos Char)
readCamera = do
  allRows <- uses rOutput (lines . map chr)
  let rows = takeWhile (not . null) allRows
      w = length $ head rows
      h = length rows
  rOutput %= drop (sum (map ((+ 1) . length) rows) + 1)
  return $ Array.listArray ((0, 0), (w - 1, h - 1)) $ concat $ transpose rows

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

intersections :: Array Pos Char -> [Pos]
intersections arr =
  let ((x1, y1), (x2, y2)) = Array.bounds arr
   in [ p
        | p <- Array.range ((x1 + 1, y1 + 1), (x2 - 1, y2 - 1)),
          let ns = neighbors p,
          all ((== '#') . (arr Array.!)) $ p : ns
      ]

part1 prog =
  let output = evalProgram prog []
      camera = evalState readCamera (Robot output)
   in sum $ map (uncurry (*)) $ intersections camera

compress xs = listToMaybe $ go [] [] xs
  where
    go :: [Int] -> [[String]] -> [String] -> [([Int], [[String]])]
    go out pats [] = do
      guard $ length out <= 10
      guard $ all (\p -> sum (map length p) + length p - 1 < 20) pats
      return (reverse out, pats)
    go out pats xs =
      let prefixPats =
            catMaybes $
              zipWith (\p i -> go (i : out) pats <$> stripPrefix p xs) pats [0 ..]
          nextPrefix = length pats
          newPrefix =
            guard (nextPrefix < 3)
              >> map
                ( \n ->
                    let (p, xs') = splitAt n xs
                     in go (nextPrefix : out) (pats ++ [p]) xs'
                )
                (reverse [1 .. 10])
       in join $ prefixPats ++ newPrefix

programPath :: String -> Maybe String
programPath = fmap format . compress . splitOn ','
  where
    splitOn x =
      unfoldr
        ( \xs ->
            if null xs
              then Nothing
              else Just $ drop 1 <$> break (== x) xs
        )
    format (result, pats) =
      let pats' = take 3 $ pats ++ repeat []
       in unlines $
            map (intercalate ",") $
              map (singleton . chr . (+ ord 'A')) result : pats'

getPath :: Array Pos Char -> String
getPath arr =
  let Just (start, dirC) = find ((`elem` "^v<>") . snd) (Array.assocs arr)
      Just dir = lookup dirC [('^', (0, -1)), ('v', (0, 1)), ('<', (-1, 0)), ('>', (1, 0))]
   in intercalate "," $ combineForward $ go start dir
  where
    go (x, y) (dx, dy) =
      let forward = try (x + dx, y + dy) (dx, dy) ["F"]
          left = try (x + dy, y - dx) (dy, -dx) ["L", "F"]
          right = try (x - dy, y + dx) (-dy, dx) ["R", "F"]
       in fromMaybe [] $ forward `mplus` left `mplus` right
    try p d cs
      | not (Array.inRange (Array.bounds arr) p) = Nothing
      | arr Array.! p /= '#' = Nothing
      | otherwise = Just $ cs ++ go p d
    combineForward cs =
      case span (== "F") cs of
        ([], c : cs') -> c : combineForward cs'
        ([], []) -> []
        (fs, cs') -> show (length fs) : combineForward cs'

part2 prog =
  let prog' = prog Vector.// [(0, 2)]
      output = evalProgram prog' input
      (camera, result) = evalState readOutput (Robot output)
      Just pathProg = programPath $ getPath camera
      input = map ord $ pathProg ++ "n\n"
   in result
  where
    readOutput = do
      camera <- readCamera
      result <- uses rOutput last
      return (camera, result)

main = do
  input <- readProgram <$> readFile "input17"
  print $ part1 input
  print $ part2 input
