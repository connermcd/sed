module Sed (sed) where

import Parser

import Control.Monad.State
import qualified Data.Text.Lazy as T
import qualified Data.List.Zipper as Z
import qualified Text.Regex as TR

data SedState = SedState {
                  line         :: Int
                , zipper       :: Z.Zipper T.Text
                , patternSpace :: T.Text
                , holdSpace    :: T.Text
                , quiet        :: Bool
                , skip         :: Bool
                }

sed :: Bool -> String -> T.Text -> T.Text
sed n s t = evalState (runCommands $ parseSed s) defaultState
    where defaultState = SedState 1 (Z.delete z) (Z.cursor z) (T.singleton '\n') n False
          z = Z.fromList $ T.lines t

runCommands :: [Command] -> State SedState T.Text
runCommands cs = do
    mapM_ runCommand cs
    -- To support laziness, we should grab the output that we
    -- have so far and make it available as a lazy Text block.
    ssWithOutput <- get
    let (Z.Zip ls rs) = zipper ssWithOutput
        outputSoFar = T.unlines $ Z.toList $ Z.Zip ls []
        remainder   = Z.Zip [] rs
    modify $ \s -> s { zipper = remainder }
    ss <- get
    if Z.endp $ zipper ss
    then do
        unless (quiet ss) (runCommand Print)
        ss <- get
        return . T.unlines . Z.toList $ zipper ss
    else do
        execute Next
        modify $ \s -> s { skip = False }
        rest <- runCommands cs
        return $ T.concat [outputSoFar, rest]

runCommand :: Command -> State SedState ()
runCommand c = gets skip >>= \skp -> unless skp (execute c)

execute :: Command -> State SedState ()
execute Print = modify $ \s -> s { zipper = Z.push (patternSpace s) (zipper s) }
execute Delete = modify $ \s -> s { skip = True }
execute Next = do
    ss <- get
    unless (quiet ss) (runCommand Print)
    if Z.endp $ zipper ss
    then modify $ \s -> s { skip = True }
    else modify $ \s -> s { line = line ss + 1,
                            zipper = Z.delete $ zipper s,
                            patternSpace = Z.cursor $ zipper s }
execute Hold = modify $ \s -> s { holdSpace = patternSpace s }
execute (Substitute regexp repl _) = modify $ \s ->
    let ps   = T.unpack . patternSpace $ s
        subs = TR.subRegex regexp ps repl
    in s { patternSpace = T.pack subs }

-- (<+>) :: T.Text -> T.Text -> T.Text
-- a <+> b = a `T.append` T.cons '\n' b
