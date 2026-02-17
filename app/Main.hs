module Main (main) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import LeanFmt (formatLean)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> TIO.getContents >>= TIO.putStr . formatLean
    paths -> mapM_ formatFileToStdout paths
  where
    formatFileToStdout :: FilePath -> IO ()
    formatFileToStdout path = do
      contents <- TIO.readFile path
      TIO.putStr (formatLean contents)

