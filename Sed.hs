module Sed where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

sed :: T.Text -> T.Text
sed t = t

main :: IO ()
main = TIO.interact sed
