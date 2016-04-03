seekritFunc ::  String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
