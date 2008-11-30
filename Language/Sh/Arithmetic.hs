{-# LANGUAGE CPP #-}
module Language.Sh.Arithmetic ( runMathParser ) where

-- This doesn't depend on any expansion at all...
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P

import Data.Bits ( shiftL, shiftR, complement, xor, (.&.), (.|.) )
import Data.List ( unionBy )
import Data.Maybe ( fromMaybe )

import Debug.Trace ( trace )

import Language.Sh.Compat ( on )

type SS = [(String,String)]
type SI = [(String,Int)]

type AP a = CharParser SS a -- just keep this state... - update when we can

data Term = Literal SI Int | Variable String | Error String
            deriving ( Show )

runMathParser :: SS -> String -> Either String (Int,SI)
runMathParser subs s = case runParser exprSubs (subs) "" s of
                         Left err -> Left $ show err
                         Right x  -> Right x

joinS :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
joinS = unionBy ((==) `on` fst)

mapS :: (b -> c) -> [(a,b)] -> [(a,c)]
mapS f = map $ \(a,b)->(a,f b)

-- after a buildExprParser, we'll check the new assignments and make them...

exprSubs :: AP (Int,SI)
exprSubs = do e <- expr
              --e <- parens (string "1+2") >> return (Literal [] 3) 
              --string "(1+2)"
              --let e = Literal [] 3
              eof
              case e of
                Literal subs i  -> return (i,subs)
                Variable s -> do ss <- getState
                                 let val = fromMaybe "0" $ lookup s ss
                                 case runMathParser ss val of
                                   Left err -> fail err
                                   Right (i,si) -> return (i,si)
                Error err -> fail err

lexer :: P.TokenParser st
lexer = P.makeTokenParser $
        emptyDef {identLetter     = alphaNum <|> char '_'
                 , opStart        = oneOf [] -- no nonreserved operators
                 , opLetter       = oneOf []
                 , reservedOpNames= ["++","+","--","-","*","/","%","^"
                                    ,"|","||","&","&&","<<",">>"
                                    ,"<","<=",">",">=","==","=","!=","!","~"
                                    ,"?",":"
                                    ,"+=","-=","*=","/=","%=","|=","&="
                                    ,"^=","<<=",">>="]
                 }

parens = P.parens lexer -- what is P?
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
decimal = P.decimal lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer

natural = do n <- octal <|> decimal <|> hexadecimal
             whiteSpace
             return n
    where octal = do char '0'
                     bo 0
          bo n = do d <- oneOf "01234567" <?> "octal digit"
                    return $ 8*n + read [d]
                 <|> return n

mapT :: (Int -> Int) -> Term -> Term
mapT _ (Error err) = Error err
mapT _ (Variable v) = Error $ "impossible: unexpanded variable: "++v
mapT f (Literal s i) = Literal s $ f i

mapT2 :: (Int -> Int -> Int) -> Term -> Term -> Term
mapT2 _ (Error err) _ = Error err
mapT2 _ (Variable v) _ = Error $ "impossible: unexpanded variable: "++v
mapT2 _ _ (Error err) = Error err
mapT2 _ _ (Variable v) = Error $ "impossible: unexpanded variable: "++v
mapT2 f (Literal s1 i1) (Literal s2 i2) = Literal (s1 `joinS` s2) $ f i1 i2

expr1 :: AP Term
expr1    = buildExpressionParser table1 term
  --      <?> "expression"

expr2 :: AP Term -- here's where we get the ternary operator
expr2 = try (do eIf <- expr1
                reservedOp "?"
                eThen <- expr1
                reservedOp ":"
                eElse <- expr1
                ss <- getState
                case expand ss eIf of
                  Error err -> return $ Error err
                  Literal si i -> return $ if (i/=0) then expandWith ss si eThen
                                                     else expandWith ss si eElse
            ) <|> expr1
    where expandWith ss si t = case expand (mapS show si `joinS` ss) t of
                                 Error err -> Error err
                                 Literal si' i -> Literal (si `joinS` si') i

expr :: AP Term
expr = buildExpressionParser table2 expr2 -- short circuit

term :: AP Term
term    = parens expr
            <|> fmap (Literal [] . fromIntegral) natural
            <|> fmap Variable identifier
          <?> "simple expression"

-- Type depends on which parsec we're using...
table1 :: OperatorTable Char SS Term
#ifdef HAVE_PARSEC_POSTFIX
table1 = [ [postfix "++" $ postinc (+1), postfix "--" $ postinc (+(-1))]
         , [prefix "+" $ e1 id, prefix "-" $ e1 negate]
         , [prefix "++" $ preinc (+1), prefix  "--" $ preinc (+(-1))]
#else
table1 = [ [prefix "+" $ e1 id, prefix "-" $ e1 negate]
#endif
         , [prefix "~" $ e1 complement,prefix "!" $ e1 $ b2i . not . i2b]
         , [binary "*" $ e2 (*), binary "/" $ e2 div, binary "%" $ e2 mod]
         , [binary "+" $ e2 (+), binary "-" $ e2 (-)]
         , [binary "<<" $ e2 shiftL, binary ">>" $ e2 shiftR]
         , [binary "<" $ e2 $ b2i .: (<), binary "<=" $ e2 $ b2i .: (<=)
           ,binary ">" $ e2 $ b2i .: (>), binary ">=" $ e2 $ b2i .: (>=)
           ,binary "==" $ e2 $ b2i .: (==), binary "!=" $ e2 $ b2i .: (/=)]
         , [binary "&" $ e2 (.&.)]
         , [binary "^" $ e2 xor]
         , [binary "|" $ e2 (.|.)]
         , [binary "&&" $ e2 $ b2 (&&)]
         , [binary "||" $ e2 $ b2 (||)] ]
    where e1 :: (Int -> Int) -> AP (Term -> Term)
          e1 f = do ss <- getState
                    return $ mapT f . expand ss
          e2 :: (Int -> Int -> Int) -> AP (Term -> Term -> Term)
          e2 f = do ss <- getState
                    return $ \t1 t2 -> mapT2 f (expand ss t1) (expand ss t2)
          b2 :: (Bool -> Bool -> Bool) -> Int -> Int -> Int
          b2 f i j = b2i $ f (i2b i) (i2b j)
          i2b i = if i==0 then False else True
          b2i b = if b then 1 else 0
          (.:) f g a b = f $ g a b -- (c -> d) -> (a -> b -> c) -> a -> b -> d
          ro name = try (reservedOp name >> notFollowedBy (char '='))
          binary name fun = Infix (ro name >> fun) AssocLeft
          prefix name fun = Prefix (reservedOp name >> fun)
#ifdef HAVE_PARSEC_POSTFIX
          postfix name fun = Postfix (ro name >> fun)
#endif

expand :: SS -> Term -> Term
expand _ (Error err) = Error err
expand _ (Literal s i) = Literal s i
expand subs (Variable name) =
    case lookup name subs of
      Nothing -> Literal [] 0
      Just s  -> case runMathParser subs s of
                   Left err -> Error err
                   Right (i,si) -> Literal si i

postinc,preinc :: (Int -> Int) -> AP (Term -> Term)
postinc f = assignReturn $ \i -> (f i,i)
preinc  f = assignReturn $ \i -> (f i,f i)

assignReturn' :: SS -> SI -> (Int -> (Int,Int)) -> (Term -> Term)
assignReturn' ss si f = ar
    where ar (Error err) = Error err
          ar (Literal _ i) = Error $ "assignment to non-variable: "++show i
          ar (Variable v) = let val = fromMaybe "0" $ lookup v ss
                            in case runMathParser ss val of
                                 Left err -> Error err
                                 Right (i,si') ->
                                     let (ass,ret) = f i
                                         si'' = [(v,ass)] `joinS` si' `joinS` si
                                     in Literal si'' ret

assignReturn :: (Int -> (Int,Int)) -> AP (Term -> Term)
assignReturn f = do ss <- getState
                    return $ assignReturn' ss [] f

assignReturn2 :: (Int -> Int -> Int) -> AP (Term -> Term -> Term)
assignReturn2 f = ar `fmap` getState
    where ar ss t t' = let t'' = expand ss t'
                       in case t'' of
                            Error err -> Error err
                            Literal si j ->
                                assignReturn' ss si (\i -> (f i j,f i j)) t

-- In between these: the ternary operator...

--table2 :: OperatorTable Char SS Int
table2 = [ [op "=" $ flip const, op "*=" (*), op "/=" div
           ,op "%=" mod, op "+=" (+), op "-=" (-)]
         , [op "<<=" shiftL, op ">>=" shiftR
           ,op "&=" (.&.), op "^=" xor, op "|=" (.|.)] ]
    where a2 :: (Int -> Int -> Int) -> AP (Term -> Term -> Term)
          a2 = assignReturn2
          op name fun = Infix (reservedOp name >> a2 fun) AssocLeft
    -- a2's first Term MUST be a string... (else "assignment to non-variable")



-- operators:
-- endTok = (`elem` " \t\r\n()+*-/%^|&")
