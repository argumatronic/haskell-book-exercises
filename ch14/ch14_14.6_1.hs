import Test.QuickCheck (sample, elements, Gen)

data Fool =
      Fulse
    | Frue
    deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFoolMoreFulse :: Gen Fool
genFoolMoreFulse = elements [Fulse, Fulse, Frue]