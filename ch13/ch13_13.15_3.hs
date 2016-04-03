import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

norm :: String -> String
norm = map toLower

palindrome :: IO ()
palindrome = forever $ do
  putStr "type text to check for palindrome: "
  line1 <- getLine
  case norm line1 == reverse (norm line1) of
    True -> do
              putStrLn "It's a palindrome!"
              exitSuccess
    False -> putStrLn "Nope!"

main :: IO ()
main = palindrome
