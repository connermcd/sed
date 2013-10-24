module Sed where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data SedState = SedState {
                  line         :: Int
                , input        :: T.Text
                , patternSpace :: T.Text
                , holdSpace    :: T.Text
                , output       :: T.Text
                }

data Command = Print
             | Delete
             | Next
             | Substitute String String [Char]

sed :: [Command] -> T.Text -> T.Text
sed cs t = evalState (runCommands cs) defaultState
    where defaultState = SedState 1 t (head $ T.lines t) T.empty T.empty

runCommands :: [Command] -> State SedState T.Text
runCommands cs = do
    mapM_ runCommand cs
    ss <- get
    if line ss == length (T.lines $ input ss)
    then return $ output ss
    else runCommand Next >> runCommands cs

runCommand :: Command -> State SedState ()
runCommand Print = modify $ \ss ->
    let newOutput = (T.lines $ output ss) ++ (T.lines $ patternSpace ss)
    in ss { output = T.unlines newOutput }
runCommand Delete = modify $ \ss -> ss { patternSpace = T.empty }
runCommand Next = modify $ \ss -> ss { line = line ss + 1, patternSpace = (T.lines $ input ss) !! line ss }

main :: IO ()
main = TIO.interact $ sed [Print]
