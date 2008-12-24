{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses,
             FlexibleInstances #-}

module Language.Sh.Map ( ExpressionMapperM(..), ExpressionMapper(..) ) where

import Language.Sh.Syntax
import Control.Monad ( ap )

concatMapM :: (Monad m,Functor m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

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
    defaultMapCommandM f (Synchronous l) = Synchronous `fmap` mapListM f l
    defaultMapCommandM f (Asynchronous l) = Asynchronous `fmap` mapListM f l

    mapListM :: f -> AndOrList -> m AndOrList
    mapListM = defaultMapListM
    defaultMapListM :: f -> AndOrList -> m AndOrList
    defaultMapListM f (Singleton p) = Singleton `fmap` mapPipelineM f p
    defaultMapListM f (l :&&: p) = (:&&:) `fmap` mapListM f l
                                          `ap` mapPipelineM f p
    defaultMapListM f (l :||: p) = (:||:) `fmap` mapListM f l
                                          `ap` mapPipelineM f p
    
    mapPipelineM :: f -> Pipeline -> m Pipeline
    mapPipelineM = defaultMapPipelineM
    defaultMapPipelineM :: f -> Pipeline -> m Pipeline
    defaultMapPipelineM f (Pipeline ps) = Pipeline `fmap`
                                          mapM (mapStatementM f) ps
    defaultMapPipelineM f (BangPipeline ps) = BangPipeline `fmap`
                                              mapM (mapStatementM f) ps

    -- do we want mapStatementsM?
    mapStatementM :: f -> Statement -> m Statement
    mapStatementM = defaultMapStatementM
    defaultMapStatementM :: f -> Statement -> m Statement
    defaultMapStatementM f (Statement ws rs as)
        = Statement `fmap` mapM (mapWordM f) ws -- plural?
                    `ap` mapM (mapRedirM f) rs
                    `ap` mapM (mapAssignmentM f) as
    defaultMapStatementM f (OrderedStatement ts)
        = OrderedStatement `fmap` concatMapM (mapTermsM f) ts
    defaultMapStatementM f (Compound c rs)
        = Compound `fmap` mapCompoundM f c `ap` mapM (mapRedirM f) rs
    defaultMapStatementM f (FunctionDefinition s c rs)
        = FunctionDefinition s `fmap` (mapCompoundM f) c
                               `ap` mapM (mapRedirM f) rs
    
    mapCompoundM :: f -> CompoundStatement -> m CompoundStatement
    mapCompoundM = defaultMapCompoundM
    defaultMapCompoundM :: f -> CompoundStatement -> m CompoundStatement
    defaultMapCompoundM f (For s ss cs') = For s `fmap` mapM (mapWordM f) ss
                                                 `ap` mapCommandsM f cs'
    defaultMapCompoundM f (If cond thn els)
        = If `fmap` mapCommandsM f cond `ap` mapCommandsM f thn
                                        `ap` mapCommandsM f els
    defaultMapCompoundM f (Subshell cs) = Subshell `fmap` mapCommandsM f cs
    defaultMapCompoundM f (BraceGroup cs) = BraceGroup `fmap` mapCommandsM f cs

    mapTermsM :: f -> Term -> m [Term]
    mapTermsM = defaultMapTermsM
    defaultMapTermsM :: f -> Term -> m [Term]
    defaultMapTermsM f t = replicate 1 `fmap` mapTermM f t

    mapTermM :: f -> Term -> m Term
    mapTermM = defaultMapTermM
    defaultMapTermM :: f -> Term -> m Term
    defaultMapTermM f (TWord w) = TWord `fmap` mapWordM f w
    defaultMapTermM f (TRedir r) = TRedir `fmap` mapRedirM f r
    defaultMapTermM f (TAssignment a) = TAssignment `fmap` mapAssignmentM f a

    mapWordM :: f -> Word -> m Word
    mapWordM = defaultMapWordM
    defaultMapWordM :: f -> Word -> m Word
    defaultMapWordM f = concatMapM $ mapLexemesM f

    mapLexemesM :: f -> Lexeme -> m [Lexeme]
    mapLexemesM = defaultMapLexemesM
    defaultMapLexemesM :: f -> Lexeme -> m [Lexeme]
    defaultMapLexemesM f l = replicate 1 `fmap` mapLexemeM f l

    mapLexemeM :: f -> Lexeme -> m Lexeme
    mapLexemeM = defaultMapLexemeM
    defaultMapLexemeM :: f -> Lexeme -> m Lexeme
    defaultMapLexemeM f (Quoted lexeme) = Quoted `fmap` mapLexemeM f lexeme
    defaultMapLexemeM f (Expand xp) = Expand `fmap` mapExpansionM f xp
    defaultMapLexemeM _ lexeme = return lexeme

    mapExpansionM :: f -> Expansion -> m Expansion
    mapExpansionM = defaultMapExpansionM
    defaultMapExpansionM :: f -> Expansion -> m Expansion
    defaultMapExpansionM f (FancyExpansion s c b w)
        = FancyExpansion s c b `fmap` mapWordM f w
    defaultMapExpansionM f (CommandSub cs) = CommandSub `fmap`
                                             mapCommandsM f cs
    defaultMapExpansionM f (Arithmetic w) = Arithmetic `fmap` mapWordM f w
    defaultMapExpansionM _ expansion = return expansion

    mapAssignmentM :: f -> Assignment -> m Assignment
    mapAssignmentM = defaultMapAssignmentM
    defaultMapAssignmentM :: f -> Assignment -> m Assignment
    defaultMapAssignmentM f (s:=w) = (s:=) `fmap` mapWordM f w

    mapRedirM :: f -> Redir -> m Redir
    mapRedirM = defaultMapRedirM
    defaultMapRedirM :: f -> Redir -> m Redir
    defaultMapRedirM f (n:>w) = (n:>) `fmap` mapWordM f w
    defaultMapRedirM f (n:>|w) = (n:>|) `fmap` mapWordM f w
    defaultMapRedirM f (n:>>w) = (n:>>) `fmap` mapWordM f w
    defaultMapRedirM f (n:<>w) = (n:<>) `fmap` mapWordM f w
    defaultMapRedirM f (n:<w) = (n:<) `fmap` mapWordM f w
    defaultMapRedirM f (Heredoc n c w) = (Heredoc n c) `fmap` mapWordM f w
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

infixl 0 $$
($$) = ($)

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
    defaultMapPipeline f (Pipeline ps) = Pipeline $ map (mapStatement f) ps
    defaultMapPipeline f (BangPipeline ps) = BangPipeline $
                                             map (mapStatement f) ps

    -- do we want mapStatementsM?
    mapStatement :: f -> Statement -> Statement
    mapStatement = defaultMapStatement
    defaultMapStatement :: f -> Statement -> Statement
    defaultMapStatement f (Statement ws rs as)
        = Statement $$ map (mapWord f) ws -- plural?
                    $$ map (mapRedir f) rs $$ map (mapAssignment f) as
    defaultMapStatement f (OrderedStatement ts)
        = OrderedStatement $ concatMap (mapTerms f) ts
    defaultMapStatement f (Compound c rs)
        = Compound $$ mapCompound f c $$ map (mapRedir f) rs
    defaultMapStatement f (FunctionDefinition s c rs)
        = FunctionDefinition s $$ (mapCompound f) c
                               $$ map (mapRedir f) rs
    
    mapCompound :: f -> CompoundStatement -> CompoundStatement
    mapCompound = defaultMapCompound
    defaultMapCompound :: f -> CompoundStatement -> CompoundStatement
    defaultMapCompound f (For s ss cs') = For s $$ map (mapWord f) ss
                                                $$ mapCommands f cs'
    defaultMapCompound f (If cond thn els)
        = If $$ mapCommands f cond $$ mapCommands f thn
                                   $$ mapCommands f els
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
    defaultMapExpansion f (FancyExpansion s c b w)
        = FancyExpansion s c b $ mapWord f w
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
