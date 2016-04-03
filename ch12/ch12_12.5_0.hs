import Data.List (intercalate)

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

replaceThe :: String -> String
replaceThe str = intercalate " " $ map athe $ fmap notThe $ words str
  where athe Nothing = "a"
        athe (Just x) = x
