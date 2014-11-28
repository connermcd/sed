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
    ss <- get
    if Z.endp $ zipper ss
    then do
        unless (quiet ss) (runCommand Print)
        ss <- get
        return . T.unlines . Z.toList $ zipper ss
    else do
        execute Next
        modify $ \s -> s { skip = False }
        runCommands cs

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
execute (Substitute p r _) = modify $ \s ->
    let regex = TR.subRegex (TR.mkRegex p) (T.unpack $ patternSpace s) r
    in s { patternSpace = T.pack regex }

-- (<+>) :: T.Text -> T.Text -> T.Text
-- a <+> b = a `T.append` T.cons '\n' b
