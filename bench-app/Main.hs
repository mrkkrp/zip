module Main (main) where

import Codec.Archive.Zip
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath

main :: IO ()
main = do
  [operation, input, output] <- getArgs
  case operation of
    "compress" -> do
      selector <- mkEntrySelector (takeFileName input)
      createArchive output (loadEntry Deflate selector input)
    "uncompress" ->
      withArchive input (unpackInto output)
    _ -> do
      putStrLn "Unknown command."
      exitFailure
