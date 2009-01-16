{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses,
             FlexibleInstances #-}

module Language.Sh.Map ( ExpressionMapperM(..), ExpressionMapper(..) ) where

import Language.Sh.Syntax
import Control.Monad ( ap )

concatMapM :: (Monad m,Functor m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- |I'm lazy and think these look a lot nicer than all the @`fmap`@s and
-- @`ap`@s and @mapM@s all over the place.  I've stolen these more or
-- less from 'Control.Applicative' and 'Control.Arrow'.
(<$>) :: Functor m => (a -> b) -> m a -> m b
(<$>) = fmap
(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
(<>) :: Monad m => (a -> m b) -> [a] -> m [b]
(<>) = mapM
(><>) :: (Monad m,Functor m) => (a -> m [b]) -> [a] -> m [b]
(><>) = concatMapM
(<***>) :: Monad m => (a -> m c) -> (b -> m d) -> (a,b) -> m (c,d)
(<***>) f g (a,b) = do a' <- f a
                       b' <- g b
                       return (a',b')
infixl 4 <$>,<*>
infixl 7 <>,><>
infixr 3 <***>


-- |The idea here is to prevent duplicating code needlessly.
-- We could go even more extreme and make a third parameter, but
-- then we have WAY too many instances, and they all depend on
-- every other one anyway...
-- class Applicative a => ExpressionMapper a f t where
--   mapSh :: f -> t -> a t

class (Monad m,Functor m) => ExpressionMapperM m f | f -> m where
    mapCommandsM :: f -> [Command] -> m [Command]
    mapCommandsM = defaultMapCommandsM
    defaultMapCommandsM :: f -> [Command] -> m [Command]
    defaultMapCommandsM f = mapM $ mapCommandM f

    mapCommandM :: f -> Command -> m Command
    mapCommandM = defaultMapCommandM
    defaultMapCommandM :: f -> Command -> m Command
    defaultMapCommandM f (Synchronous l) = Synchronous <$> mapListM f l
    defaultMapCommandM f (Asynchronous l) = Asynchronous <$> mapListM f l

    mapListM :: f -> AndOrList -> m AndOrList
    mapListM = defaultMapListM
    defaultMapListM :: f -> AndOrList -> m AndOrList
    defaultMapListM f (Singleton p) = Singleton <$> mapPipelineM f p
    defaultMapListM f (l :&&: p) = (:&&:) <$> mapListM f l
                                          <*> mapPipelineM f p
    defaultMapListM f (l :||: p) = (:||:) <$> mapListM f l
                                          <*> mapPipelineM f p
    
    mapPipelineM :: f -> Pipeline -> m Pipeline
    mapPipelineM = defaultMapPipelineM
    defaultMapPipelineM :: f -> Pipeline -> m Pipeline
    defaultMapPipelineM f (Pipeline ps) = Pipeline <$> mapStatementM f <> ps
    defaultMapPipelineM f (BangPipeline ps) = BangPipeline <$>
                                              mapStatementM f <> ps

    -- do we want mapStatementsM?
    mapStatementM :: f -> Statement -> m Statement
    mapStatementM = defaultMapStatementM
    defaultMapStatementM :: f -> Statement -> m Statement
    defaultMapStatementM f (Statement ws rs as)
        = Statement <$> mapWordM f <> ws -- plural?
                    <*> mapRedirM f <> rs <*> mapAssignmentM f <> as
    defaultMapStatementM f (OrderedStatement ts)
        = OrderedStatement <$> mapTermsM f ><> ts
    defaultMapStatementM f (Compound c rs)
        = Compound <$> mapCompoundM f c <*> mapRedirM f <> rs
    defaultMapStatementM f (FunctionDefinition s c rs)
        = FunctionDefinition s <$> mapCompoundM f c <*> mapRedirM f <> rs
    
    mapCompoundM :: f -> CompoundStatement -> m CompoundStatement
    mapCompoundM = defaultMapCompoundM
    defaultMapCompoundM :: f -> CompoundStatement -> m CompoundStatement
    defaultMapCompoundM f (For s ss cs') = For s <$> mapWordM f <> ss
                                                 <*> mapCommandsM f cs'
    defaultMapCompoundM f (While cond code) = While <$> mapCommandsM f cond
                                                    <*> mapCommandsM f code
    defaultMapCompoundM f (Until cond code) = Until <$> mapCommandsM f cond
                                                    <*> mapCommandsM f code
    defaultMapCompoundM f (If cond thn els)
        = If <$> mapCommandsM f cond <*> mapCommandsM f thn
                                     <*> mapCommandsM f els
    defaultMapCompoundM f (Case expr cases)
        = Case <$> mapWordM f expr
               <*> ((mapWordM f <>) <***> mapCommandsM f) <> cases
    defaultMapCompoundM f (Subshell cs) = Subshell <$> mapCommandsM f cs
    defaultMapCompoundM f (BraceGroup cs) = BraceGroup <$> mapCommandsM f cs

    mapTermsM :: f -> Term -> m [Term]
    mapTermsM = defaultMapTermsM
    defaultMapTermsM :: f -> Term -> m [Term]
    defaultMapTermsM f t = replicate 1 <$> mapTermM f t

    mapTermM :: f -> Term -> m Term
    mapTermM = defaultMapTermM
    defaultMapTermM :: f -> Term -> m Term
    defaultMapTermM f (TWord w) = TWord <$> mapWordM f w
    defaultMapTermM f (TRedir r) = TRedir <$> mapRedirM f r
    defaultMapTermM f (TAssignment a) = TAssignment <$> mapAssignmentM f a

    mapWordM :: f -> Word -> m Word
    mapWordM = defaultMapWordM
    defaultMapWordM :: f -> Word -> m Word
    defaultMapWordM f = concatMapM $ mapLexemesM f

    mapLexemesM :: f -> Lexeme -> m [Lexeme]
    mapLexemesM = defaultMapLexemesM
    defaultMapLexemesM :: f -> Lexeme -> m [Lexeme]
    defaultMapLexemesM f l = replicate 1 <$> mapLexemeM f l

    mapLexemeM :: f -> Lexeme -> m Lexeme
    mapLexemeM = defaultMapLexemeM
    defaultMapLexemeM :: f -> Lexeme -> m Lexeme
    defaultMapLexemeM f (Quoted lexeme) = Quoted <$> mapLexemeM f lexeme
    defaultMapLexemeM f (Expand xp) = Expand <$> mapExpansionM f xp
    defaultMapLexemeM _ lexeme = return lexeme

    mapExpansionM :: f -> Expansion -> m Expansion
    mapExpansionM = defaultMapExpansionM
    defaultMapExpansionM :: f -> Expansion -> m Expansion
    defaultMapExpansionM f (ModifiedExpansion s c b w)
        = ModifiedExpansion s c b <$> mapWordM f w
    defaultMapExpansionM f (CommandSub cs) = CommandSub <$> mapCommandsM f cs
    defaultMapExpansionM f (Arithmetic w) = Arithmetic <$> mapWordM f w
    defaultMapExpansionM _ expansion = return expansion

    mapAssignmentM :: f -> Assignment -> m Assignment
    mapAssignmentM = defaultMapAssignmentM
    defaultMapAssignmentM :: f -> Assignment -> m Assignment
    defaultMapAssignmentM f (s:=w) = (s:=) <$> mapWordM f w

    mapRedirM :: f -> Redir -> m Redir
    mapRedirM = defaultMapRedirM
    defaultMapRedirM :: f -> Redir -> m Redir
    defaultMapRedirM f (n:>w) = (n:>) <$> mapWordM f w
    defaultMapRedirM f (n:>|w) = (n:>|) <$> mapWordM f w
    defaultMapRedirM f (n:>>w) = (n:>>) <$> mapWordM f w
    defaultMapRedirM f (n:<>w) = (n:<>) <$> mapWordM f w
    defaultMapRedirM f (n:<w) = (n:<) <$> mapWordM f w
    defaultMapRedirM f (Heredoc n c w) = (Heredoc n c) <$> mapWordM f w
    defaultMapRedirM _ redir = return redir

instance (Monad m,Functor m) => ExpressionMapperM m (Command -> m Command)
    where mapCommandM f c = defaultMapCommandM f =<< f c

instance (Monad m,Functor m) => ExpressionMapperM m (AndOrList -> m AndOrList)
    where mapListM f l = defaultMapListM f =<< f l

instance (Monad m,Functor m) => ExpressionMapperM m (Pipeline -> m Pipeline)
    where mapPipelineM f p = defaultMapPipelineM f =<< f p

instance (Monad m,Functor m) => ExpressionMapperM m (Statement -> m Statement)
    where mapStatementM f s = defaultMapStatementM f =<< f s

instance (Monad m,Functor m)
    => ExpressionMapperM m (CompoundStatement -> m CompoundStatement)
    where mapCompoundM f s = defaultMapCompoundM f =<< f s

instance (Monad m,Functor m) => ExpressionMapperM m (Word -> m Word)
    where mapWordM f w = defaultMapWordM f =<< f w

instance (Monad m,Functor m) => ExpressionMapperM m (Lexeme -> m Lexeme)
    where mapLexemeM f l = defaultMapLexemeM f =<< f l

instance (Monad m,Functor m) => ExpressionMapperM m (Lexeme -> m [Lexeme])
    where mapLexemesM f l =  f =<< defaultMapLexemeM f l

instance (Monad m,Functor m) => ExpressionMapperM m (Expansion -> m Expansion)
    where mapExpansionM f x = defaultMapExpansionM f =<< f x

instance (Monad m,Functor m) => ExpressionMapperM m (Assignment -> m Assignment)
    where mapAssignmentM f a = defaultMapAssignmentM f =<< f a

instance (Monad m,Functor m) => ExpressionMapperM m (Redir -> m Redir)
    where mapRedirM f r = defaultMapRedirM f =<< f r

($$) = ($)
(!) = map
(>!) = concatMap
(***) f g (a,b) = (f a,g b)
infixl 0 $$
infixl 7 !, >!
infixr 3 ***

class ExpressionMapper f where
    mapCommands :: f -> [Command] -> [Command]
    mapCommands = defaultMapCommands
    defaultMapCommands :: f -> [Command] -> [Command]
    defaultMapCommands f = map $ mapCommand f

    mapCommand :: f -> Command -> Command
    mapCommand = defaultMapCommand
    defaultMapCommand :: f -> Command -> Command
    defaultMapCommand f (Synchronous l) = Synchronous $ mapList f l
    defaultMapCommand f (Asynchronous l) = Asynchronous $ mapList f l

    mapList :: f -> AndOrList -> AndOrList
    mapList = defaultMapList
    defaultMapList :: f -> AndOrList -> AndOrList
    defaultMapList f (Singleton p) = Singleton $ mapPipeline f p
    defaultMapList f (l :&&: p) = mapList f l :&&: mapPipeline f p
    defaultMapList f (l :||: p) = mapList f l :||: mapPipeline f p
    
    mapPipeline :: f -> Pipeline -> Pipeline
    mapPipeline = defaultMapPipeline
    defaultMapPipeline :: f -> Pipeline -> Pipeline
    defaultMapPipeline f (Pipeline ps) = Pipeline $ mapStatement f ! ps
    defaultMapPipeline f (BangPipeline ps) = BangPipeline $
                                             mapStatement f ! ps

    -- do we want mapStatementsM?
    mapStatement :: f -> Statement -> Statement
    mapStatement = defaultMapStatement
    defaultMapStatement :: f -> Statement -> Statement
    defaultMapStatement f (Statement ws rs as)
        = Statement $$ mapWord f ! ws -- plural?
                    $$ mapRedir f ! rs $$ mapAssignment f ! as
    defaultMapStatement f (OrderedStatement ts)
        = OrderedStatement $ mapTerms f >! ts
    defaultMapStatement f (Compound c rs)
        = Compound $$ mapCompound f c $$ mapRedir f ! rs
    defaultMapStatement f (FunctionDefinition s c rs)
        = FunctionDefinition s $$ mapCompound f c $$ mapRedir f ! rs
    
    mapCompound :: f -> CompoundStatement -> CompoundStatement
    mapCompound = defaultMapCompound
    defaultMapCompound :: f -> CompoundStatement -> CompoundStatement
    defaultMapCompound f (For s ss cs') = For s $$ mapWord f ! ss
                                                $$ mapCommands f cs'
    defaultMapCompound f (While cond code) = While $$ mapCommands f cond
                                                   $$ mapCommands f code
    defaultMapCompound f (Until cond code) = Until $$ mapCommands f cond
                                                   $$ mapCommands f code
    defaultMapCompound f (If cond thn els)
        = If $$ mapCommands f cond $$ mapCommands f thn $$ mapCommands f els
    defaultMapCompound f (Case expr cases)
        = Case $$ mapWord f expr $$ ((mapWord f !) *** mapCommands f) ! cases
    defaultMapCompound f (Subshell cs) = Subshell $ mapCommands f cs
    defaultMapCompound f (BraceGroup cs) = BraceGroup $ mapCommands f cs

    mapTerms :: f -> Term -> [Term]
    mapTerms = defaultMapTerms
    defaultMapTerms :: f -> Term -> [Term]
    defaultMapTerms f t = [mapTerm f t]

    mapTerm :: f -> Term -> Term
    mapTerm = defaultMapTerm
    defaultMapTerm :: f -> Term -> Term
    defaultMapTerm f (TWord w) = TWord $ mapWord f w
    defaultMapTerm f (TRedir r) = TRedir $ mapRedir f r
    defaultMapTerm f (TAssignment a) = TAssignment $ mapAssignment f a

    mapWord :: f -> Word -> Word
    mapWord = defaultMapWord
    defaultMapWord :: f -> Word -> Word
    defaultMapWord f = concatMap $ mapLexemes f

    mapLexemes :: f -> Lexeme -> [Lexeme]
    mapLexemes = defaultMapLexemes
    defaultMapLexemes :: f -> Lexeme -> [Lexeme]
    defaultMapLexemes f l = [mapLexeme f l]

    mapLexeme :: f -> Lexeme -> Lexeme
    mapLexeme = defaultMapLexeme
    defaultMapLexeme :: f -> Lexeme -> Lexeme
    defaultMapLexeme f (Quoted lexeme) = Quoted $ mapLexeme f lexeme
    defaultMapLexeme f (Expand xp) = Expand $ mapExpansion f xp
    defaultMapLexeme _ lexeme = lexeme

    mapExpansion :: f -> Expansion -> Expansion
    mapExpansion = defaultMapExpansion
    defaultMapExpansion :: f -> Expansion -> Expansion
    defaultMapExpansion f (ModifiedExpansion s c b w)
        = ModifiedExpansion s c b $ mapWord f w
    defaultMapExpansion f (CommandSub cs) = CommandSub $ mapCommands f cs
    defaultMapExpansion f (Arithmetic w) = Arithmetic $ mapWord f w
    defaultMapExpansion _ expansion = expansion

    mapAssignment :: f -> Assignment -> Assignment
    mapAssignment = defaultMapAssignment
    defaultMapAssignment :: f -> Assignment -> Assignment
    defaultMapAssignment f (s:=w) = s := mapWord f w

    mapRedir :: f -> Redir -> Redir
    mapRedir = defaultMapRedir
    defaultMapRedir :: f -> Redir -> Redir
    defaultMapRedir f (n:>w) =  n :>  mapWord f w
    defaultMapRedir f (n:>|w) = n :>| mapWord f w
    defaultMapRedir f (n:>>w) = n :>> mapWord f w
    defaultMapRedir f (n:<>w) = n :<> mapWord f w
    defaultMapRedir f (n:<w) =  n :<  mapWord f w
    defaultMapRedir f (Heredoc n c w) = Heredoc n c $ mapWord f w
    defaultMapRedir _ redir = redir

instance ExpressionMapper (Command -> Command)
    where mapCommand f c = defaultMapCommand f $ f c

instance ExpressionMapper (AndOrList -> AndOrList)
    where mapList f l = defaultMapList f $ f l

instance ExpressionMapper (Pipeline -> Pipeline)
    where mapPipeline f p = defaultMapPipeline f $ f p

instance ExpressionMapper (Statement -> Statement)
    where mapStatement f s = defaultMapStatement f $ f s

instance ExpressionMapper (CompoundStatement -> CompoundStatement)
    where mapCompound f s = defaultMapCompound f $ f s

instance ExpressionMapper (Word -> Word)
    where mapWord f w = defaultMapWord f $ f w

instance ExpressionMapper (Lexeme -> Lexeme)
    where mapLexeme f l = defaultMapLexeme f $ f l

instance ExpressionMapper (Lexeme -> [Lexeme])
    where mapLexemes f l = f $ defaultMapLexeme f l

instance ExpressionMapper (Expansion -> Expansion)
    where mapExpansion f x = defaultMapExpansion f $ f x

instance ExpressionMapper (Assignment -> Assignment)
    where mapAssignment f a = defaultMapAssignment f $ f a

instance ExpressionMapper (Redir -> Redir)
    where mapRedir f r = defaultMapRedir f $ f r
