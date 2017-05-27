module Main (main) where

import Sed
import qualified Data.Text.Lazy as T

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

-- Here we create an input consisting of ten valid lines plus one
-- (final) line that will cause an error when evaluated.  We  use
-- this input to make sure that the sed function does not consume
-- any more input then it needs  to  produce the amount of output
-- that we ask it for. This test uses the sed function  to  simu-
-- late  running  `yes | sed -n 'p' | head 1` at the command line
-- to verify that it terminates.
lazyCase :: IO Test
lazyCase = do
    let input  = replicate 10 "y" ++ [error "Non-Laziness Detected"]
    let result = sed True "p" (T.unlines $ map T.pack $ input)
    mapM_ print $ map T.unpack $ take 1 $ T.lines $ result
    return $ testCase "lazyCase" (assertBool "" True)

main :: IO ()
main = do
    loud <- mapM (sedCase False text) tests
    quiet <- mapM (sedCase True text) tests
    lazy <- lazyCase
    defaultMain [testGroup "sed" loud, testGroup "sed -n" quiet, lazy]
