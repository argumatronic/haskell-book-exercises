class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- error
-- Illegal instance declaration for ‘TooMany GoatInt’
--       (All instance types must be of the form (T t1 ... tn)
--        where T is not a synonym.
--        Use TypeSynonymInstances if you want to disable this.)
--     In the instance declaration for ‘TooMany GoatInt’
--
-- type GoatInt = Int
-- instance TooMany GoatInt where
--   tooMany n = n > 44
