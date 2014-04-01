module Main where

import Sed
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    TIO.interact $ sed (head args)
