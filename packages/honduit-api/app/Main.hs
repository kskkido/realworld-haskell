module Main
  ( main
  ) where

import RIO
import qualified System.IO as IO

main :: IO.IO ()
main = do
  IO.putStrLn $ "Running"
