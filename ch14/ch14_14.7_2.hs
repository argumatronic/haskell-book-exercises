import Data.Char (ord, chr, isAlpha)
import Test.QuickCheck (quickCheck, verboseCheck, Gen)

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x:xs) n
  | isAlpha x = docyp x n (+) : caesar xs n
  | otherwise = x : caesar xs n


unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar (x:xs) n
  | isAlpha x = docyp x n (-) : unCaesar xs n
  | otherwise = x: unCaesar xs n

docyp :: Char -> Int -> (Int -> Int -> Int) -> Char
docyp x n f = chr $ f (ord x - base) n `mod` range + base
  where base = ord 'a'
        range = 26


-- vigenere "MEET AT DAWN" "ALLY" = "ALLY AL LYAL"

vigenere :: String -> String -> String
vigenere [] _ = ""
vigenere xs ys = vigenere' xs (cycle (map ord ys)) where
  vigenere' (x:xs) cyp@(n:ns)
    | isAlpha x = docyp x n (+) : vigenere' xs ns
      | otherwise = x : vigenere' xs cyp

unVigenere :: String -> String -> String
unVigenere [] _ = ""
unVigenere xs ys = unVigenere' xs (cycle (map ord ys)) where
  unVigenere' (x:xs) cyp@(n:ns)
    | isAlpha x = docyp x n (+) : unVigenere' xs ns
      | otherwise = x : unVigenere' xs cyp


-- capitalGen :: Gen Char
-- capitalGen = elements [A..Z]

-- propCaesar :: Property
-- propCaesar =
--   forAll capitalGen
--   (\c -> (unCaesar (caesar text shift) shift) == text)


propCaesar :: String -> Int -> Bool
propCaesar text shift = (unCaesar (caesar text shift) shift) == text

propVigenere :: String -> String -> Bool
propVigenere text code = (unVigenere (vigenere text code) code) == text

main :: IO ()
main = do
  verboseCheck propCaesar
  verboseCheck propVigenere