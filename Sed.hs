module Sed (sed) where

import Parser

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.List.Zipper as Z
import qualified Text.Regex as TR

data SedState = SedState {
                  line         :: Int
                , zipper       :: Z.Zipper T.Text
                , patternSpace :: T.Text
                , holdSpace    :: T.Text
                }

sed :: Bool -> String -> T.Text -> T.Text
sed n s t = evalState (runCommands n cs) defaultState
    where cs = parseSed s
          defaultState = SedState 1 (Z.delete z) (Z.cursor z) (T.singleton '\n')
          z = Z.fromList $ T.lines t

runCommands :: Bool -> [Command] -> State SedState T.Text
runCommands n cs = do
    mapM_ runCommand cs
    unless n (runCommand Print)
    ss <- get
    if Z.endp $ zipper ss
    then return . T.unlines . Z.toList $ zipper ss
    else runCommand Next >> runCommands n cs

runCommand :: Command -> State SedState ()
runCommand Print = modify $ \ss -> ss { zipper = Z.push (patternSpace ss) (zipper ss) }
runCommand Delete = modify $ \ss -> ss { patternSpace = T.empty }
runCommand Next = modify $ \ss -> ss { line = line ss + 1,
                                       zipper = Z.delete $ zipper ss,
                                       patternSpace = Z.cursor $ zipper ss }
runCommand (Substitute p r _) = modify $ \ss ->
    let regex = TR.subRegex (TR.mkRegex p) (T.unpack $ patternSpace ss) r
    in ss { patternSpace = T.pack regex }

-- (<+>) :: T.Text -> T.Text -> T.Text
-- a <+> b = a `T.append` T.cons '\n' b
