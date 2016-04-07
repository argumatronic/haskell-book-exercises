data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty                      = Nada
  mappend Nada      (Only a)  = Only a
  mappend Nada      Nada      = Nada
  mappend (Only a)  Nada      = Only a
  mappend (Only a)  (Only b)  = Only (mappend a b)