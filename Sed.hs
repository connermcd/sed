module Sed where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List.Zipper as Z

data SedState = SedState {
                  line         :: Int
                , input        :: Z.Zipper T.Text
                , patternSpace :: T.Text
                , holdSpace    :: T.Text
                -- , output       :: T.Text
                }

data Command = Print
             | Delete
             | Next
             | Substitute String String [Char]

sed :: [Command] -> T.Text -> T.Text
sed cs t = evalState (runCommands cs) defaultState
    where defaultState = SedState 1 (Z.delete z) (Z.cursor z) (T.singleton '\n')
          z = Z.fromList $ T.lines t

runCommands :: [Command] -> State SedState T.Text
runCommands cs = do
    mapM_ runCommand cs
    ss <- get
    if Z.endp $ input ss
    then return . T.unlines . Z.toList $ input ss
    else runCommand Next >> runCommands cs

runCommand :: Command -> State SedState ()
runCommand Print = modify $ \ss -> ss { input = Z.push (patternSpace ss) (input ss) }
runCommand Delete = modify $ \ss -> ss { patternSpace = T.empty }
runCommand Next = modify $ \ss -> ss { line = line ss + 1,
                                       input = Z.delete $ input ss,
                                       patternSpace = Z.cursor $ input ss }

-- (<+>) :: T.Text -> T.Text -> T.Text
-- a <+> b = a `T.append` T.cons '\n' b

main :: IO ()
main = TIO.interact $ sed [Print]
