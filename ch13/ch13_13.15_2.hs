import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  putStr "type text to check for palindrome: "
  line1 <- getLine
  case line1 == reverse line1 of
    True -> do
              putStrLn "It's a palindrome!"
              exitSuccess
    False -> putStrLn "Nope!"

main :: IO ()
main = palindrome
