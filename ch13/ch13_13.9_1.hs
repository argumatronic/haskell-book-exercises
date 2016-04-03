main :: IO Bool
main = do c <- getChar
          c' <- getChar
          return (c == c')