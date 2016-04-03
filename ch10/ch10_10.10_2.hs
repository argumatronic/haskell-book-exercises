seekritFunc ::  Fractional a => String -> a
seekritFunc x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
