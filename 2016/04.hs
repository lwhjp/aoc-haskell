import Data.Char
import Data.Either
import Data.List
import Data.Ord
import Text.Parsec

data Room = Room
  { roomName :: [String],
    roomSector :: Int,
    roomChecksum :: String
  }

readRoom = fromRight (error "parse error") . runParser room () ""
  where
    room = do
      name <- many lower `endBy` char '-'
      sector <- read <$> many1 digit
      checksum <- between (char '[') (char ']') $ many lower
      return $ Room name sector checksum

checksum = take 5 . map head . sortOn (Down . length) . group . sort . filter isLetter

isReal (Room name _ check) = check == checksum (concat name)

decrypt k = unwords . map rotate
  where
    rotate = map $ \c -> chr $ ord 'a' + ((ord c - ord 'a' + k) `mod` 26)

main = do
  rooms <- map readRoom . lines <$> readFile "input04"
  let realRooms = filter isReal rooms
      decrypted = map (\(Room n s _) -> (decrypt s n, s)) realRooms
  print $ sum $ map roomSector realRooms
  print $ lookup "northpole object storage" decrypted
