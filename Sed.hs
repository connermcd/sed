module Sed where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List.Zipper as Z
import qualified Text.Regex as TR

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
             | Substitute String String String

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
runCommand (Substitute p r _) = modify $ \ss ->
    let regex = TR.subRegex (TR.mkRegex p) (T.unpack $ patternSpace ss) r
    in ss { patternSpace = T.pack regex }

-- (<+>) :: T.Text -> T.Text -> T.Text
-- a <+> b = a `T.append` T.cons '\n' b

main :: IO ()
main = TIO.interact $ sed [Substitute "^t(.*)" "\\1t" "", Print]
