\chapter{Expression module}

\begin{code}

module System.Console.ShSh.Expression ( Expression(..), Redir,
                                        parseExpr, mapExpr, mapExprM,
                                        concatMapExprM ) where

import System.Console.ShSh.Lexer ( Lexeme(..), Token(..) )
import System.Console.ShSh.Builtins ( BuiltinCommand(..), toBuiltin )
import System.Console.ShSh.Redirection ( Redir(..) )
import System.Console.ShSh.Operator ( Operator(..) )

import Control.Monad ( liftM2 )

data Expression = Simple [Token]
                | Cmd String [String] [Redir] -- ...?
                | Builtin BuiltinCommand [String] [Redir]
                | SubShell Expression
                | Expression :|: Expression
                | Expression :&&: Expression
                | Expression :||: Expression
                | Expression :>>: Expression -- semicolon
                | RunAsync Expression
                deriving ( Show )

infixl 4 :>>:
infixl 5 :||:,:&&:
infixr 6 :|:

mapExpr :: ([Token] -> [Token]) -> Expression -> Expression
mapExpr f (Simple xs) = Simple $ f xs
mapExpr f (SubShell e) = SubShell $ mapExpr f e
mapExpr f (e :|: e') = mapExpr f e :|: mapExpr f e'
mapExpr f (e :&&: e') = mapExpr f e :&&: mapExpr f e'
mapExpr f (e :||: e') = mapExpr f e :||: mapExpr f e'
mapExpr f (e :>>: e') = mapExpr f e :>>: mapExpr f e'
mapExpr f (RunAsync e) = RunAsync $ mapExpr f e
mapExpr _ x = x

mapExprE :: ([Token] -> Expression) -> Expression -> Expression
mapExprE f (Simple xs) = f xs
mapExprE f (SubShell e) = SubShell $ mapExprE f e
mapExprE f (e :|: e') = mapExprE f e :|: mapExprE f e'
mapExprE f (e :&&: e') = mapExprE f e :&&: mapExprE f e'
mapExprE f (e :||: e') = mapExprE f e :||: mapExprE f e'
mapExprE f (e :>>: e') = mapExprE f e :>>: mapExprE f e'
mapExprE f (RunAsync e) = RunAsync $ mapExprE f e
mapExprE _ x = x

mapExprM :: (Monad m, Functor m) => ([Token] -> m [Token])
                                 -> Expression -> m Expression
mapExprM f (Simple xs) = Simple `fmap` f xs
mapExprM f (SubShell e) = SubShell `fmap` mapExprM f e
mapExprM f (e :|: e') = liftM2 (:|:) (mapExprM f e) (mapExprM f e')
mapExprM f (e :&&: e') = liftM2 (:&&:) (mapExprM f e) (mapExprM f e')
mapExprM f (e :||: e') = liftM2 (:||:) (mapExprM f e) (mapExprM f e')
mapExprM f (e :>>: e') = liftM2 (:>>:) (mapExprM f e) (mapExprM f e')
mapExprM f (RunAsync e) = RunAsync `fmap` mapExprM f e
mapExprM _ x = return x

mapExprME :: (Monad m, Functor m) => ([Token] -> m Expression)
                                 -> Expression -> m Expression
mapExprME f (Simple xs) = f xs
mapExprME f (SubShell e) = SubShell `fmap` mapExprME f e
mapExprME f (e :|: e') = liftM2 (:|:) (mapExprME f e) (mapExprME f e')
mapExprME f (e :&&: e') = liftM2 (:&&:) (mapExprME f e) (mapExprME f e')
mapExprME f (e :||: e') = liftM2 (:||:) (mapExprME f e) (mapExprME f e')
mapExprME f (e :>>: e') = liftM2 (:>>:) (mapExprME f e) (mapExprME f e')
mapExprME f (RunAsync e) = RunAsync `fmap` mapExprME f e
mapExprME _ x = return x

concatMapExprM :: (Monad m, Functor m) => (Token -> m [Token])
                                       -> Expression -> m Expression
concatMapExprM f = mapExprM $ concatMapM f

-- This is duplicated...
fromLiteral :: Monad m => [Lexeme] -> m String
fromLiteral = mapM $ \x -> do {Literal c <- return x; return c}

toStr :: Monad m => [Token] -> m ([String], [Redir])
toStr [] = return ([], [])
toStr (x:xs) = case x of
               Word x' -> case fromLiteral x' of
                          Nothing -> fail $ "unexpanded "++show x'
                                          ++": either it's unsupported, "
                                          ++"or a bug."
                          Just x'' -> do (xs',redirs) <- toStr xs
                                         return (x'':xs', redirs)
               x' -> do redirs <- toRedirs (x:xs)
                        return ([],redirs)

toRedirs :: Monad m => [Token] -> m ([Redir])
toRedirs [] = return []
toRedirs (Oper Great:Word outf:xs) = do rest <- toRedirs xs
                                        outf' <- fromLiteral outf
                                        return (OutTo outf':rest)
toRedirs (Oper Less:Word inf:xs) = do rest <- toRedirs xs
                                      inf' <- fromLiteral inf
                                      return (InFrom inf':rest)
toRedirs (Oper DGreat:Word apf:xs) = do rest <- toRedirs xs
                                        apf' <- fromLiteral apf
                                        return (AppendTo apf':rest)
toRedirs xs = fail $ "Weird redirection: "++ show xs

parseExpr :: (Monad m, Functor m) => Expression -> m Expression
parseExpr = mapExprME $ fmap f . toStr
    where f :: ([String],[Redir]) -> Expression
          f (x:xs,redirs) = case toBuiltin x of
                            Just b  -> Builtin b xs redirs
                            Nothing -> Cmd x xs redirs
          f ([],[]) = Builtin Exec [] [] -- no-op...
          f ([],redirs) = error $ "redirection with no command? "++ show redirs

concatMapM :: (Monad m, Functor m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat `fmap` mapM f xs

\end{code}
