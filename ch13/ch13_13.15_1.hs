import Data.Char (ord, chr, isAlpha)

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
docyp x n f = chr $ f (ord x - base) n `mod` r + base
  where base = ord 'a'
        r = 26

t = "abc xyz"
shift = 5

cipherOk = unCaesar (caesar t shift) shift == t

main :: IO ()
main = do
  putStr "input caesar shift: "
  cyp <- getLine
  putStr "input text: "
  str <- getLine
  putStrLn $ "encrypted text: " ++ caesar str (read cyp :: Int)
