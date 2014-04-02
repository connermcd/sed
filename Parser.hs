module Parser (Command(..), parseSed) where

import Data.Char (isAlpha)
import Text.Parsec
import Text.Parsec.String

data Command = Print
             | Delete
             | Next
             | Hold
             | Substitute String String String
             deriving Show

delims :: String
delims = "!@#$%^&*-_=+'\";:,./?|"

parseSed :: String -> [Command]
parseSed s = case parse parseCommands [] s of
                Left _ -> undefined
                Right cmds -> cmds

parseCommands :: Parser [Command]
parseCommands = sepEndBy parseCommand (char ';' <|> newline)

parseCommand :: Parser Command
parseCommand = do
    many $ char ' '
    cmd <- parsePrint <|> parseDelete <|> parseNext <|> parseHold <|> parseSubstitute
    many $ char ' '
    return cmd

parsePrint, parseDelete, parseNext, parseHold, parseSubstitute :: Parser Command
parsePrint = char 'p' >> return Print
parseDelete = char 'd' >> return Delete
parseNext = char 'n' >> return Next
parseHold = char 'h' >> return Hold
parseSubstitute = do
    char 's'
    delim <- oneOf delims
    pattern <- many $ satisfy (/= delim)
    char delim
    replace <- many $ satisfy (/= delim)
    char delim
    flags <- many $ satisfy isAlpha
    return (Substitute pattern replace flags)
