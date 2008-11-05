\chapter{Expression module}

\begin{code}

module System.Console.ShSh.Expression ( Expression(..), Redir,
                                        parseExpr, mapExpr, mapExprM ) where

import System.Console.ShSh.Lexer ( Lexeme(..), Token(..) )
import System.Console.ShSh.Builtins ( BuiltinCommand, toBuiltin )
import System.Console.ShSh.Redirection ( Redir )

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

-- This is duplicated...
fromLiteral :: [Lexeme] -> Maybe String
fromLiteral = mapM $ \x -> do {Literal c <- return x; return c}

toStr :: Monad m => [Token] -> m [String]
toStr = mapM $ \x -> case x of
                       Word x' -> case fromLiteral x' of
                                    Nothing -> fail $ "unexpanded "++show x'
                                                 ++": either it's unsupported, "
                                                 ++"or a bug."
                                    Just x'' -> return x''
                       x' -> fail $ "impossible "++show x'++" in toStr."

parseExpr :: (Functor m, Monad m) => Expression -> m Expression
parseExpr = mapExprME $ fmap f . toStr
    where f :: [String] -> Expression
          f (x:xs) = case toBuiltin x of
                       Just b  -> Builtin b xs []
                       Nothing -> Cmd x xs []

\end{code}
