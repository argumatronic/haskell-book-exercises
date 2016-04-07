import Test.QuickCheck (quickCheck, verboseCheck)

square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt

squareProp x = squareIdentity x == x

main = do
  quickCheck squareProp
