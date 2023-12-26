{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Monad
import Data.Array (Array)
import Data.Array.IArray
import Data.Either
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec

data RegexpPart = Chars [Char] | Branch [Regexp]

type Regexp = [RegexpPart]

parseRegexp :: String -> Regexp
parseRegexp = fromRight (error "parse error") . runParser fullRegexp () ""
  where
    fullRegexp = between (char '^') (char '$') regexp
    regexp = many part
    part = try branch <|> chars
    chars = Chars <$> many1 letter
    branch = Branch <$> between (char '(') (char ')') (regexp `sepBy` char '|')

type Pos = (Int, Int)

expandPoints :: Regexp -> Set Pos
expandPoints = Set.fromList . (start :) . go start
  where
    start = (0, 0)
    go p [] = []
    go p (Chars cs : rest) =
      let ps = tail $ scanl' step p (double cs)
       in ps ++ go (last ps) rest
    go p (Branch parts : rest) =
      let branches = map (go p) parts
          ends = nub $ map (last . (p :)) branches
       in concat branches ++ concatMap (`go` rest) ends
    double [] = []
    double (x : xs) = x : x : double xs
    step (y, x) c =
      case c of
        'N' -> (y - 1, x)
        'E' -> (y, x + 1)
        'S' -> (y + 1, x)
        'W' -> (y, x - 1)

type Maze = Array Pos Bool

buildMaze :: Regexp -> Maze
buildMaze regexp = accumArray (const id) True extent $ map (,False) points
  where
    points = Set.elems $ expandPoints regexp
    ((y1, y2), (x1, x2)) = join (***) (minimum &&& maximum) $ unzip points
    extent = ((y1 - 1, x1 - 1), (y2 + 1, x2 + 1))

dists :: Set Pos -> [(Pos, Int)]
dists points = go 0 Set.empty $ Set.singleton (0, 0)
  where
    go d visited front
      | Set.null front = []
      | otherwise =
          let neighbors (y, x) =
                [ p
                  | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
                    let p = (y + dy, x + dx),
                    p `Set.notMember` visited,
                    p `Set.member` points
                ]
              visited' = Set.union visited front
              front' = Set.fromList $ concatMap neighbors front
           in map (,d) (Set.elems front) ++ go (d + 1) visited' front'

roomDists :: Set Pos -> [Int]
roomDists =
  mapMaybe
    ( \(_, d) ->
        case quotRem d 2 of
          (q, 0) -> Just q
          _ -> Nothing
    )
    . dists

main = do
  input <- parseRegexp <$> readFile "input20"
  let dists = roomDists $ expandPoints input
  print $ last dists
  print $ length $ dropWhile (< 1000) dists
