import Test.QuickCheck (sample, elements, frequency, Gen)

data Fool =
      Fulse
    | Frue
    deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFoolMoreFulse :: Gen Fool
genFoolMoreFulse = frequency [(2, return Fulse),
                              (1, return Frue)]