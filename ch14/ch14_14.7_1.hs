import Test.QuickCheck (sample, oneof, frequency, Gen)

data Fool =
      Fulse
    | Frue
    deriving (Eq, Show)

genFool :: Gen Fool
genFool = oneof [Fulse, Frue]

genFoolMoreFulse :: Gen Fool
genFoolMoreFulse = frequency [(2, return Fulse),
                              (1, return Frue)]