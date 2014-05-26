module Main (main) where

import Sed
import qualified Data.Text as T

import Control.Monad
import Data.List
import System.Process
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

text :: String
text = "test\nthis\nout\n"

cmds :: String
cmds = "pnd"

tests :: [String]
tests = map (intersperse ';') (nub . join . map subsequences . replicateM 3 $ cmds)

sedCase :: Bool -> String -> String -> IO Test
sedCase n txt cs = do
    let cmd = if n then ["-n", cs] else [cs]
    gnuSed <- readProcess "sed" cmd txt
    return $ testCase (show cs) $ sed n cs (T.pack txt) @?= T.pack gnuSed

main :: IO ()
main = do
    loud <- mapM (sedCase False text) tests
    quiet <- mapM (sedCase True text) tests
    defaultMain [testGroup "sed" loud, testGroup "sed -n" quiet]
