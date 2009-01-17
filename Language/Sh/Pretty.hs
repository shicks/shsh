{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
             PatternGuards #-}

module Language.Sh.Pretty ( Pretty, pretty,
                            compacted, indented ) where

import Control.Monad.State ( State, runState, evalState,
                             get, gets, put, modify )
import Data.Char ( isAlphaNum )

import Language.Sh.Syntax

-- |This is the pretty-printing class
class Pretty p where
    pretty :: p -> String -- ^The standard user pretty-printing function.
    -- |This version takes extra context information as arguments.
    prettyC :: p -> PrettyP String 

    pretty p = evalState (prettyC p) empty
    prettyC = return . pretty

instance Pretty String where pretty = id
instance Pretty (PrettyP String) where prettyC = id

-- helper types
data QuoteType = None | DoubleQ | SingleQ | HeredocQ
                 deriving ( Show, Eq )
type PrettyP = State PState
data PState = PState { heredocs :: [(Word,String)]
                     , delims   :: String
                     , quotes   :: QuoteType
                     , indent   :: Int
                     , compact  :: Bool } deriving ( Show )
empty :: PState
empty = PState [] normalDelimiters None 0 False

-- Just reset the things we mess with at the word level...
reset :: PrettyP ()
reset = modify $ \s -> s { delims = normalDelimiters, quotes = None }

-- helper functions (all internal)
setQuote :: QuoteType -> PrettyP ()
setQuote q = modify $ \s -> s { quotes = q }

sub :: PrettyP a -> PrettyP a
sub a = do s <- get
           normalDelims
           put $ s { heredocs = [], quotes = None }
           a' <- a
           modify $ \s' -> s { heredocs = heredocs s++heredocs s' }
           return a'

runLater :: Pretty p => p -> PrettyP (String,PState)
runLater a = do s <- get
                return $ runState (prettyC a) $ s { heredocs = [] }

runNow :: Pretty p => PState -> p -> PrettyP String
runNow s a = do a' <- prettyC a
                laterState s
                return a'

laterState :: PState -> PrettyP ()
laterState s = modify $ \s' -> s { heredocs = heredocs s'++heredocs s }

normalDelimiters :: String
normalDelimiters = "&|;<>()# \t\r\n"

normalDelims, braceDelims, noDelims :: PrettyP ()
normalDelims = modify $ \s -> s { delims = "&|;<>()# \t\r\n" }
braceDelims = modify $ \s -> s { delims = "}" }
noDelims = modify $ \s -> s { delims = "" }

-- This has a different implementation because we'll be a bit
-- smarter about quotes: this should still work after quote removal
-- The monad makes order a bit more important, but as long as we're
-- within a single word, we can rearrange...?  just need to make
-- sure the heredocs go in the right spot...
quote :: QuoteType -> PrettyP String
quote q' = do q <- gets quotes
              if q==HeredocQ
                then return ""
                else do modify $ \s -> s { quotes = q' }
                        return $ switch q
    where switch q | q == q' = ""
                   | otherwise = symb q' ++symb q
          symb SingleQ = "'"
          symb DoubleQ = "\""
          symb _ = ""

-- turn off quoting if the same type is on...
toggleQuote :: QuoteType -> PrettyP String
toggleQuote q' = do q <- gets quotes
                    if q==HeredocQ
                       then return ""
                       else if q'==q then quote None
                                     else quote q'

-- turn on quoting only if it's not already on
quoteOn :: PrettyP String
quoteOn = do q <- gets quotes
             case q of
               None -> quote DoubleQ
               _ -> return ""

(+++) :: (Pretty p,Pretty q) => p -> q -> PrettyP String
p +++ q = do p' <- prettyC p
             q' <- prettyC q
             return $ p'++q'

-- inserts any pending heredocs where they'd be asked for
-- argument is what to print if we're compact ("; " or " ")
newline :: String -> PrettyP String
newline x = do ind <- gets indent
               c <- gets compact
               if c then return x
                    else printHD +++ ("\n" ++ replicate ind ' ')

printHD :: PrettyP String
printHD = do hds <- gets heredocs
             modify $ \s -> s { heredocs = [] }
             intercM "" (map showhd hds) -- foldM?
    where showhd (w,s) = "\n" +++ heredoc w +++ term s
          term "" = ""
          term s  = "\n"++s
          heredoc w = do q <- gets quotes
                         modify $ \s -> s { quotes = HeredocQ }
                         w' <- prettyC w
                         modify $ \s -> s { quotes = q }
                         return w'

-- This is only used BETWEEN words, so we can reset!
intercM :: (Pretty p,Pretty q) => q -> [p] -> PrettyP String
intercM _ [] = return []
intercM _ [x] = prettyC x
intercM s (x:xs) = prettyC x +++ prettyC s +++ intercM s xs

indented :: Pretty p => Int -> p -> PrettyP String
indented i p = do s <- get
                  put $ s { indent = indent s + i }
                  a <- prettyC p
                  modify $ \s' -> s' { indent = indent s }
                  return a

compacted :: Pretty p => p -> PrettyP String
compacted p = do s <- get
                 put $ s { compact = True }
                 a <- prettyC p
                 modify $ \s' -> s' { compact = compact s }
                 return a

-- This is a pretty dumb printer.  As long as all the quote
-- characters are retained, it should work fine.  But we
-- won't really end up using it.

instance Pretty Lexeme where
    prettyC (Literal c) = return [c]
    prettyC (Quoted l) = prettyC l
    prettyC (Quote q) = return [q]
    prettyC (Expand x) = prettyC x -- this is why we need prettyC here
    prettyC SplitField = return ""

-- This is where most of the trickness happens.
instance Pretty [Lexeme] where
    prettyC w = do s <- pc w
                   reset
                   return s
     where
      pc [] = return ""
      pc (Quote '\'':ls) = toggleQuote SingleQ +++ pc ls
      pc (Quote '"':ls) = toggleQuote DoubleQ +++ pc ls
      pc (Quote _:ls) = pc ls
      pc (Quoted l:ls) = quoteOn +++ pc' (l:ls)
      pc (l:ls) = quote None +++ pc' (l:ls)
      -- Once we've got the quotes right...
      pc' (Quoted l:ls) = pc' $ l:ls
      pc' (Quote _:ls) = pc ls
      -- Now we've only got Expand, Literal, or SplitField
      pc' (Expand (SimpleExpansion x):ls) | (c:cs) <- x
          = do (rest,st) <- runLater $ pc ls
               if (null cs && c `elem` "@*#?-$!0123456789")
                  || null rest || not (isAlphaUnderNum (head rest))
                  then (runNow st $ '$':x) +++ rest
                  else (runNow st $ SimpleExpansion x) +++ rest
      pc' (Expand x:ls) = prettyC x +++ pc ls
      pc' (Literal c:ls) = do q <- gets quotes
                              d <- gets delims
                              lit q d c +++ pc ls
      pc' (SplitField:ls) = do q <- gets quotes
                               quote None +++ " " +++ quote q
      lit q d c = case q of -- backslash-quote anything we need to
                    None     | c `elem` (d++"$`\"'\\") -> ['\\',c]
                    SingleQ  | c == '\''               -> "'\"'\"'"
                    DoubleQ  | c `elem` "$`\"\\"       -> ['\\',c]
                    HeredocQ | c `elem` "$`\\"         -> ['\\',c]
                    _ -> [c]
      isAlphaUnderNum c = isAlphaNum c || c=='_'

instance Pretty Expansion where
    prettyC (SimpleExpansion s) = return $ "${" ++ s ++ "}"
    prettyC (ModifiedExpansion s c b w)
        = do w' <- sub $ braceDelims >> compacted w
             return $ "${" ++ s ++ oper ++ w' ++ "}"
        where oper | c `elem` "-=+?" = (if b then (':':) else id) [c]
                   | c `elem` "#%" = (if b then (c:) else id) [c]
                   | otherwise = [c] -- error?
    prettyC (LengthExpansion s) = return $ "${#" ++ s ++ "}"
    prettyC (CommandSub cs) = sub $ "$( " +++ compacted cs +++ " )"
    prettyC (Arithmetic w) = sub $ do noDelims
                                      "$(( " +++ compacted w +++ " ))"

instance Pretty Redir where
    prettyC = p where
        p (n:>w)  = pn 1 n ">" +++ w
        p (n:>|w) = pn 1 n ">|" +++ w
        p (n:>&m) = pn 1 n ">&" +++ show m
        p (n:>>w) = pn 1 n ">>" +++ w
        p (n:<>w) = pn 0 n "<>" +++ w
        p (n:<w)  = pn 0 n "<"  +++ w
        p (n:<&m) = pn 0 n "<&" +++ show m
        p (n:<<s) = pn 0 n "<<" +++ s
        p (n:<<-s) = pn 0 n "<<-" +++ s
        p (Heredoc n _ w) -- we're losing info here... but only a little
            = do modify $ \s -> s { heredocs = heredocs s ++ [(w,eot)] }
                 pn 0 n "<<" +++ eot
            where eot = "EOF"++show (length w) -- "random"?
        pn def n s | n == def  = s++" "
                   | otherwise = show n++" "++s++" "

instance Pretty Assignment where
    prettyC (s:=w) = (s++"=") +++ prettyC w

------ now upwards...

instance Pretty CompoundStatement where
    prettyC = pc where
      pc (For name ws cs) = "for " +++ name +++ " in "
                            +++ intercM " " ws +++ loop cs
      pc (While cond cs) = "while " +++ compacted cond +++ loop cs
      pc (Until cond cs) = "until " +++ compacted cond +++ loop cs
      pc (If cond thn els) = "if " +++ compacted cond  +++ "; then"
                             +++ indented 2 (newline "; "+++thn)
                             +++ elseblock
          where elseblock = case els of
                              [] -> newline "; " +++ "fi"
                              [Synchronous (Singleton (Pipeline [
                                Compound i@(If _ _ _) []]))]
                                  -> newline "; " +++ "el" +++ pc i
                              _ -> newline "; " +++ "else"
                                   +++ indented 2 (newline " " +++ els)
                                   +++ newline "; " +++ "fi"
      pc (Case w ss) = "case " +++ w +++ " in" +++ indented 2 (cas ss)
                       +++ newline " " +++ "esac"
          where cas [] = return ""
                cas ((ps,cs):xs)
                    = newline " "+++ "(" +++ intercM "|" ps +++ ")"
                      +++ indented 2 (newline " " +++ cs +++ ";;")
                      +++ cas xs
      pc (Subshell cs) = "(" +++ indented 2 cs +++ ")"
      pc (BraceGroup cs) = "{ " +++ indented 2 cs +++ newline "; " +++ "}"
      loop cs = "; do" +++ indented 2 (newline " " +++ cs)
                +++ newline "; " +++ "done"

instance Pretty Term where
    prettyC (TWord w) = prettyC w
    prettyC (TRedir r) = prettyC r
    prettyC (TAssignment a) = prettyC a

instance Pretty Statement where
    prettyC (Statement ws rs as) = intercM " " as +++ space as ws
                                   +++ intercM " " ws +++ space ws rs
                                   +++ intercM " " rs
        where space as bs = if null as || null bs then "" else " "
    prettyC (OrderedStatement ts) = intercM " " ts
    prettyC (Compound c rs) = c +++ " " +++ intercM " " rs
    prettyC (FunctionDefinition f d rs)
        = case d of -- specialize in the case of { }
            BraceGroup b -> f +++ " () {" +++ indented 2 (newline " " +++ b)
                            +++ newline "; " +++ "} " +++ intercM " " rs
            _ -> f +++ " ()" +++ newline " " +++ d +++ intercM " " rs

instance Pretty Pipeline where
    prettyC (Pipeline ss) = intercM " | " ss
    prettyC (BangPipeline ss) = "! " +++ intercM " | " ss

instance Pretty AndOrList where
    prettyC (Singleton p) = prettyC p
    prettyC (a :&&: p) = a +++ " && " +++ p
    prettyC (a :||: p) = a +++ " || " +++ p

instance Pretty Command where
    prettyC (Synchronous a) = prettyC a +++ printHD
    prettyC (Asynchronous a) = a +++ " &" +++ printHD

instance Pretty [Command] where
    prettyC [] = return ""
    prettyC [c] = prettyC c
    prettyC (Synchronous a:cs) = a +++ newline "; " +++ cs
    prettyC (Asynchronous a:cs) = a +++ " &" +++ newline " " +++ cs
