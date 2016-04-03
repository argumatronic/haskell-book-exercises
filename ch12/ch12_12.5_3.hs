newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = if lv < lc then Just (Word' w) else Nothing
  where lv = length $ filter ((flip elem) vowels) w
        lc = length w - lv
