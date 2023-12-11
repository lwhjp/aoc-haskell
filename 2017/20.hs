{-# LANGUAGE LambdaCase #-}

import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Text.Parsec

data Vec = Vec !Int !Int !Int deriving (Eq, Ord)

data Particle = Particle {pId :: !Int, pPos :: !Vec, pVel :: !Vec, pAcc :: !Vec}

readParticle :: Int -> String -> Particle
readParticle id = fromRight (error "parse error") . parse particle ""
  where
    particle = do
      [p, v, a] <- (anyChar >> char '=' >> vec) `sepBy` string ", "
      return $ Particle id p v a
    vec = do
      [x, y, z] <- between (char '<') (char '>') (num `sepBy` char ',')
      return $ Vec x y z
    num = read <$> many1 (oneOf "-0123456789")

stepParticle (Particle id p v a) =
  let v' = v `vecPlus` a
      p' = p `vecPlus` v'
   in Particle id p' v' a
  where
    vecPlus (Vec a b c) (Vec d e f) = Vec (a + d) (b + e) (c + f)

runParticles f = until stable $ sortOn (mnorm . pPos) . f . map stepParticle
  where
    stable ps = sortedOn (mnorm . pVel) ps && sortedOn (mnorm . pAcc) ps
    sortedOn f = and . (zipWith (<=) <*> tail) . map f
    mnorm (Vec x y z) = abs x + abs y + abs z

part1 = pId . head . runParticles id

part2 = length . runParticles removeColliding
  where
    removeColliding =
      mapMaybe (\case [p] -> Just p; _ -> Nothing)
        . groupBy ((==) `on` pPos)
        . sortOn pPos

main = do
  input <- zipWith readParticle [0 ..] . lines <$> readFile "input20"
  print $ part1 input
  print $ part2 input
