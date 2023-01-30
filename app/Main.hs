module Main where

import           Data.Semigroup      ((<>))
import Lib ( mergeSortM )
import           Options.Applicative
import System.IO (hFlush, stdout)

params :: Parser String
params = argument str (metavar "FILE")

fetchLines :: FilePath -> IO [String]
fetchLines = (fmap . fmap) lines readFile

-- TODO Give a better UX for making a decision.
comparison :: String -> String -> IO Ordering
comparison a b = do
  putStr (a ++ " <> " ++ b ++ ": ")
  hFlush stdout
  choice <- getLine
  case choice of
    "h" -> return GT
    "l" -> return LT
    _ -> do
      putStrLn "Invalid Option. Please use h to pick the left option and l for the right."
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
