import Text.JSON

numbers :: JSValue -> [Int]
numbers (JSObject obj) = concatMap (numbers . snd) $ fromJSObject obj
numbers (JSArray arr) = concatMap numbers arr
numbers (JSRational _ n) = [truncate n]
numbers _ = []

noRed :: JSValue -> JSValue
noRed (JSObject obj)
  | any (isRed . snd) entries = makeObj []
  | otherwise = makeObj $ map (fmap noRed) entries
  where
    entries = fromJSObject obj
    isRed (JSString s) = fromJSString s == "red"
    isRed _ = False
noRed (JSArray arr) = JSArray $ map noRed arr
noRed v = v

main = do
  (Ok input) <- decode <$> readFile "input12"
  print $ sum $ numbers input
  print $ sum $ numbers $ noRed input
