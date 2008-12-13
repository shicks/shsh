-- |Here we define the /complete/ abstract syntax tree for
-- simple and compound statements.

module Language.Sh.Syntax where

-- *The statement level and above
data Command = Synchronous  AndOrList
             | Asynchronous AndOrList
             deriving ( Show )
data AndOrList = Singleton Pipeline
               | AndOrList :&&: Pipeline
               | AndOrList :||: Pipeline
                 deriving ( Show )
data Pipeline = Pipeline [Statement] -- explicit type-level non-null?
              | BangPipeline [Statement]
                deriving ( Show )
data Statement = Statement [Word] [Redir] [Assignment]
               | Compound CompoundStatement [Redir]
               | FunctionDefinition String CompoundStatement [Redir]
               deriving ( Show )
data CompoundStatement = For String [Word] [Command]
                       | If [Command] [Command] [Command] -- etc...
                       | Subshell [Command]
                       | BraceGroup [Command]
                       deriving ( Show )

-- *The word level and below
type Word = [Lexeme]
data Lexeme = Literal Char | Quote Char
            | Expand Expansion | Quoted Lexeme
            deriving ( Show )
data Expansion = SimpleExpansion String
               | FancyExpansion String Char Bool Word
               | LengthExpansion String
               | CommandSub [Command]
               | Arithmetic Word
               deriving ( Show )
data Redir = Int :> Word  -- tests show that expansions don't lose spaces
           | Int :>| Word -- i.e. $ A='abc def'
           | Int :>& Int  --      $ echo 1 > $A  # target is 'abc def'
           | Int :>> Word
           | Int :<> Word
           | Int :< Word
           | Int :<& Int
           | Int :<< String
           | Int :<<- String
           | Heredoc Int Bool Word -- ^filled in version...?
           deriving ( Show )
data Assignment = String := Word
                  deriving ( Show )

--data GlobChar = Lit Char | One | Many | OneOf String | NoneOf String
--type Glob = [GlobChar]

{- Heredoc test:
$ a=$(echo -e '\t\ta')
$ echo "$a"
-e              a
$ a=$(echo '\t\ta')
$ echo "$a"
                a
$ cat <<EOF
> $a
> EOF
                a
$ cat <<-EOF
> $a
> EOF
                a
-----> so tab removal occurs at parse time, NOT at expansion time
-}