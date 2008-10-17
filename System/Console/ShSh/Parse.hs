--  Copyright (C) 2004 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module System.Console.ShSh.Parse ( Command(..), parseLine, builtinHelp )
    where

import System.Console.ShSh.Builtins ( BuiltinCommand(..) )

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Command = Cmd [String]
             | Builtin BuiltinCommand [String]
             | Command :&&: Command
             | Command :>>: Command
             | Command :|: Command
             | EmptyCommand
               deriving ( Show )

parseLine :: String -> Either String Command
parseLine s = case parse command_parser "command" s of
              Left err -> Left $ show err
              Right x -> Right x

command_parser :: CharParser st Command
command_parser = do m <- subcommand
                    eof
                    return m

subcommand :: CharParser st Command
subcommand = buildExpressionParser table cmd <?> "command thingy"

table :: OperatorTable Char st Command
table   = [ [binary "|" (:|:),
             binary "&&" (:&&:),
             binary ";" (:>>:) ]
          ]
    where binary name fun =
              Infix (do trystring name
                        spaces
                        return fun) AssocLeft

trystring :: String -> CharParser st String
trystring s = try $ string s

cmd :: CharParser st Command
cmd = between spaces spaces
      (parens subcommand
       <|> choice parseCmds_
       <|> externalCmd
       <?> "simple cmd")
    where parseCmds_ = map createHelper builtinCommands

createHelper :: (String, String, [String] -> Command)
             -> CharParser st Command
createHelper (key,_,thiscmd) =
  do trystring key
     spaces
     q <- many quoted
     return $ thiscmd q

builtinHelp :: String
builtinHelp = unlines $ map blurb builtinCommands
  where blurb :: (String, String, [String] -> Command) -> String
        blurb (c, help, _) = "   "++c ++ ": " ++ help

builtinCommands :: [(String, String, [String] -> Command)]
builtinCommands = [ ("ls", "list directory contents", Builtin Ls ),
                    ("cd", "change directory", Builtin Cd ),
                    ("pwd", "print working directory", Builtin Pwd ),
                    ("exit", "quit", Builtin Exit ),
                    ("set", "set primitives", Builtin Set ) ]

externalCmd :: CharParser st Command
externalCmd = Cmd `fmap` many1 quoted

parens :: CharParser st Command -> CharParser st Command
parens p  = between (string "(") (string ")") p

quoted :: CharParser st String
quoted = between (char '"') (char '"')
                 (many $ do { char '\\' -- allow escapes
                            ; try (oneOf ['\\', '"']) <|> return '\\'
                            }
                         <|>  noneOf ['"'])
         <|> between spaces spaces (many1 $ noneOf " ;|&()\"")
         <?> "string"
