main :: IO Bool
main = do c <- getChar
          c' <- getChar
          c == c'