import Control.Arrow
import Data.Either
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map

type Layout = ([(Int, (Int, Int))], Map Int Int)

readInput :: String -> Layout
readInput =
  map (read . singleton) . head . lines
    >>> (scanl' (+) 0 >>= zip) -- list of (pos, len)
    >>> zipWith ($) (intersperse Right [Left . (id,) | id <- [0 ..]])
    >>> partitionEithers
    >>> filter ((> 0) . snd . snd) *** Map.filter (> 0) . Map.fromAscList

checksum :: Layout -> Int
checksum = sum . map (\(id, (pos, len)) -> id * len * (2 * pos + len - 1) `div` 2) . fst

compact :: (Int -> Int -> Bool) -> Layout -> Layout
compact select (files, spaces) = foldr moveFile ([], spaces) files
  where
    moveFile file@(fileId, (filePos, fileLen)) (files, spaces) =
      let candidates = Map.assocs $ fst . Map.split filePos $ spaces
       in case find (select fileLen . snd) candidates of
            Just (spacePos, spaceLen) ->
              let spaces' = Map.delete spacePos spaces
               in if spaceLen >= fileLen
                    then
                      ( (fileId, (spacePos, fileLen)) : files,
                        if spaceLen == fileLen
                          then spaces'
                          else Map.insert (spacePos + fileLen) (spaceLen - fileLen) spaces'
                      )
                    else
                      moveFile
                        (fileId, (filePos, fileLen - spaceLen))
                        ((fileId, (spacePos, spaceLen)) : files, spaces')
            Nothing -> (file : files, spaces)

main = do
  input <- readInput <$> readFile "input09"
  mapM_ (print . checksum . ($ input) . compact) [const $ const True, (<=)]
