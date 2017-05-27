module Main where

import Sed

import Control.Monad
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import qualified Data.Text.Lazy.IO as TIO

data Flag = Help
          | Quiet
          | Expression String
          | Script String
          deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
    Option "h" ["help"]
        (NoArg Help)
        "displays this message",
    Option "n" ["quiet"]
        (NoArg Quiet)
        "suppresses automatic printing of the pattern space",
    Option "e" ["expression"]
        (ReqArg Expression "EXPRESSION")
        "adds the expression to the commands to be executed",
    Option "f" ["file"]
        (ReqArg Script "SCRIPT")
        "adds expressions from the given script"
    ]

getExpression :: Flag -> IO String
getExpression flag =
    case flag of
        Expression s -> return s
        Script s     -> readFile s
        _            -> return ""

main :: IO ()
main = do
    gotArgs <- getArgs
    (flags, args, _) <- return $ getOpt RequireOrder options gotArgs

    when (Help `elem` flags) $ do
        putStrLn (usageInfo "USAGE:" options)
        exitSuccess

    expression <- mapM getExpression flags
    let exe = if all null expression
              then head args
              else intercalate ";" $ filter (not . null) expression

    TIO.interact $ sed (Quiet `elem` flags) exe
