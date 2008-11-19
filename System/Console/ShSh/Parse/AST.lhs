\chapter{Parse.AST module}

Here we define the \emph{complete} abstract syntax tree for
simple and compound statements.

\begin{code}

module System.Console.ShSh.Parse.AST where

import Data.Monoid ( Monoid, mempty, mappend )
import Data.Maybe ( isJust )

import Debug.Trace ( trace )

impossible = const undefined

data Command = Synchronous  AndOrList
             | Asynchronous AndOrList
             | For String [String] [Command] -- ...etc
             deriving ( Show )
data AndOrList = Singleton Pipeline
               | AndOrList :&&: Pipeline
               | AndOrList :||: Pipeline
               deriving ( Show )
data Pipeline = Pipeline [Statement] -- explicit type-level non-null?
                deriving ( Show )
data Statement = Statement [Word] [Redir] [Assignment]
               | Builtin BuiltinCommand [Word] [Redir] [Assignment]
               | Subshell [Command] [Redir]
               deriving ( Show )
data Word = LitWord String | GenWord [Lexeme]
            deriving ( Show )
data Lexeme = Literal Char | Quote Char
            | Expand Expansion | Quoted Lexeme
            deriving ( Show )
data Expansion = SimpleExpansion String
               | FancyExpansion String Char Bool Word
               | LengthExpansion String
               | CommandSub [Command]
               | Arithmetic [Word]
               deriving ( Show )
data Redir = Int :> Word  -- tests show that expansions don't lose spaces
           | Int :>| Word -- i.e. $ A='abc def'
           | Int :>& Int  --      $ echo 1 > $A  # target is 'abc def'
           | Int :>> Word
           | Int :<> Word
           | Int :< Word
           | Int :<& Int
           | Int :<< Word
           | Int :<<- Word
           | Heredoc Int String -- ^filled in version...?
           deriving ( Show )
data Assignment = String := Word
                  deriving ( Show )

-- |This doesn't seem like it quite belongs here...
data BuiltinCommand = Cat | Cd | Echo | Exec | Exit | Fals
                    | Grep | Ls | MkDir | Pwd | Set | Tru
                    | SetVarInternal
                    deriving ( Eq, Show )

toBuiltin :: String -> Maybe BuiltinCommand
toBuiltin = flip lookup [("cat",Cat),("cd",Cd)
                        ,("echo",Echo),("exec",Exec),("exit",Exit)
                        ,("false",Fals),("grep",Grep),("ls",Ls)
                        ,("mkdir",MkDir),("pwd",Pwd)
                        ,("set",Set),("true",Tru)
                        ]
                         
addAssignment :: Assignment -> Statement -> Statement
addAssignment a (Statement ws rs as) = Statement ws rs (a:as)
addAssignment _ (Subshell _ _) = impossible "cannot add assignment to subshell"

addWord :: Word -> Statement -> Statement
addWord w (Statement ws rs as) = Statement (w:ws) rs as
addWord _ (Subshell _ _) = impossible "cannot add word to subshell"

addRedirection :: Redir -> Statement -> Statement
addRedirection r (Statement ws rs as) = Statement ws (r:rs) as
addRedirection r (Subshell cs rs) = Subshell cs (r:rs)

fromLiteral :: Word -> Maybe String
fromLiteral (LitWord s) = Just s
fromLiteral (GenWord []) = Just []
fromLiteral (GenWord (Literal c:cs)) = fmap (c:) $ fromLiteral $ GenWord cs
fromLiteral _ = Nothing

ql :: Char -> Lexeme
ql = Quoted . Literal

instance Monoid Word where
    mempty = LitWord ""
    mappend a b | isJust la && isJust lb = let Just a' = la
                                               Just b' = lb
                                           in LitWord $ a'++b'
                | otherwise = let GenWord a' = toGenWord a
                                  GenWord b' = toGenWord b
                              in GenWord $ a'++b'
                where la = fromLiteral a
                      lb = fromLiteral b
                      toGenWord (LitWord s) = GenWord $ map Literal s
                      toGenWord (GenWord w) = GenWord w

{-
-- Here's an example of use (explaining associativity):
runAOL :: AndOrList -> Shell Bool
runAOL (Singleton p) = runPipeline p
runAOL (l :&&: p) = do r1 <- runAOL l
                       case r1 of
                         True -> runPipeline p
                         False -> return False
runAOL (l :||: p) = do r1 <- runAOL l
                       case r1 of
                         False -> runPipeline p
                         True -> return True
-- This will, of course, lead to lots of recursion for really complex
-- statements, which could possibly be avoided with right
-- associativity (although this would lead to wrong results), or
-- something like the following:
data AOL = AOL [AOC]
data AOC = AndC Pipeline | OrC Pipeline -- first musy be AndC
runAOL [] = undefined
runAOL [aoc] = let p = getPipeline aoc in runPipeline p
runAOL (a:as) = do r1 <- let (_,p) = toBP a in runPipeline p
                   runAOL' r1 as
runAOL' b [] = return b
runAOL' b (a:as) = if b==b' then do b''<-runPipeline a
                                    runAOL' b'' as
                            else runAOL b as 

toBP :: AOC -> (Bool,Pipeline)
toBP (AndC p) = (True,p)
toBP (OrC p)  = (False,p)

-}

\end{code}
