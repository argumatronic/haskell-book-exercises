data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello b) = a == b
    (==) (Goodbye a) (Goodbye b) = a == b
    (==) _ _ = False