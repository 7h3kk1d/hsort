module Main where

import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative

params :: Parser String
params = argument str (metavar "FILE")

fetchLines :: FilePath -> IO [String]
fetchLines = (fmap . fmap) lines readFile

-- TODO Give a better UX for making a decision.
comparison :: String -> String -> IO Ordering
comparison a b = do
  putStrLn (a ++ " <> " ++ b)
  choice <- getLine
  case choice of
    "h" -> return GT
    "l" -> return LT
    _ -> do
      putStrLn "Invalid Option"
      comparison a b

main :: IO ()
main = do
  file <- execParser (info params (progDesc "The human sort"))
  contents <- readFile file
  let fs = lines contents
  ls <- mergeSortM comparison fs
  putStrLn ""
  putStrLn "Sorted: "
  mapM_ putStrLn ls
