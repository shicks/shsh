{-# OPTIONS_GHC -Wall #-}

module System.Console.ShSh.Builtins.Test ( test, brackets ) where

import Control.Monad ( MonadPlus(..) )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Error ( catchError )
import System.Directory ( doesFileExist, doesDirectoryExist,
                          canonicalizePath, getModificationTime,
                          getPermissions, readable, writable,
                          executable, searchable )
import System.Exit ( ExitCode(..) )
import System.IO ( openFile, hClose, hFileSize, IOMode(..) )

import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.ShellError ( failWith )

-- We'd like to use parsec, but I think it's too hard, since the best type
-- would be @eval :: P Bool@, but this requires IO actions to be performed.
-- ... or we'll just write our own parsec in 20 minutes.

test :: [String] -> Shell ExitCode
test ss =
    do r <- runParser (disjunction `finally` eof) ss
       case r of
            Left  s     -> failWith 2 $ "test: "++s
            Right True  -> return ExitSuccess
            Right False -> return $ ExitFailure 1

brackets :: [String] -> Shell ExitCode
brackets ss =
    do r <- runParser (disjunction `finally` (literal "]" >> eof)) ss
       case r of
         Left  s     -> failWith 2 $ "[: "++s
         Right True  -> return ExitSuccess
         Right False -> return $ ExitFailure 1

-- We could give priorities to the errors and then keep the
-- one with higher priority...?
type UnP a = Shell (Either String (a,[String]))
newtype P a = P { unP :: [String] -> UnP a }
instance Monad P where
    (>>=) a f = P $ \ss -> do r <- unP a ss
                              case r of
                                Left s -> return $ Left s
                                Right (a',ss') -> unP (f a') ss'
    return a  = P $ \ss -> return $ Right (a,ss)
    fail msg  = P $ const $ return $ Left msg

instance MonadPlus P where -- needed for msum = choice
    mzero = fail ""
    mplus a b = P $ \ss -> do r <- unP a ss
                              case r of
                                Left _  -> unP b ss
                                Right x -> return $ Right x

(<|>) :: P a -> P a -> P a
(<|>) = mplus

runParser :: P a -> [String] -> Shell (Either String a)
runParser (P p) ss = do r <- p ss
                        case r of
                             Left s -> return $ Left s
                             Right (a,_) -> return $ Right a

return' :: a -> [String] -> UnP a
return' a ss = return $ Right (a,ss)

fail' :: String -> UnP a
fail' = return . Left

finally :: P a -> P b -> P a
finally a b = do { a' <- a; b; return a' }

io :: IO a -> P a
io a = P $ \ss -> do a' <- liftIO a
                     return' a' ss
                  `catchError` (fail' . show)

eof :: P Bool
eof = P $ \ss -> case ss of
                   []    -> return' True []
                   _     -> fail' "too many arguments"

literal :: String -> P ()
literal s = P $ \ss -> case ss of
                         (s':ss') | s==s'     -> return' () ss'
                                  | otherwise -> expected s s'
                         []                   -> missing s
    where expected e f = fail' $ "`"++e++"' expected, found `"++f++"'"
          missing e = fail' $ "missing `"++e++"'"

anyString :: P String
anyString = P $ \ss -> case ss of
                         (s':ss') -> return' s' ss'
                         []       -> fail' "unexpected eof"

-- we could turn this into <?> without too much trouble...
-- (by storing an @Either String String@ as the error type)
oneOf :: String -> [(String,a)] -> P a
oneOf err ss = do s <- anyString
                  case lookup s ss of
                    Just a  -> return a
                    Nothing -> fail $ s++": "++err++" expected"

parens :: P a -> P a
parens a = do literal "("
              a' <- a
              literal ")"
              return a'

disjunction :: P Bool
disjunction = (do a <- conjunction
                  literal "-o"
                  b <- disjunction
                  return $ a || b) <|> conjunction

conjunction :: P Bool
conjunction = (do a <- term
                  literal "-a"
                  b <- conjunction
                  return $ a && b) <|> term

inv :: P Bool
inv = do literal "!"
         t <- term
         return $ not t

term :: P Bool
term = inv <|> binary <|> unary <|> single <|> parens disjunction

single :: P Bool
single = do s <- anyString
            return $ not $ null s

binary :: P Bool
binary = do a <- anyString
            op <- oneOf "binary operator" binOps
            b <- anyString
            io $ op a b

unary :: P Bool
unary = do op <- oneOf "unary operator" unOps
           a <- anyString
           io $ op a

binOps :: [(String,String -> String -> IO Bool)]
binOps = [("-nt",newerThan),("-ot",flip newerThan),("-ef",sameFile),
          ("=",return2 (==)),("!=",return2 (/=)),
          ("<",return2 (<)),(">",return2 (>)),
          ("-eq",int (==)),("-ne",int (/=)),("-lt",int (<)),
          ("-le",int (<=)),("-gt",int (>)),("-ge",int (>))]
    where return2 f a b = return $ f a b
          int f a b = return $ f (read a::Int) (read b::Int)

unOps :: [(String,String -> IO Bool)]
unOps = [("-b",isBlockFile),("-c",isCharFile),("-d",doesDirectoryExist),
         ("-e",doesFileExist),("-f",isRegularFile),("-g",isSetGid),
         ("-h",isSymlink),("-k",isSticky),("-n",r $ not.null),
         ("-p",isFipo),("-r",isReadable),("-s",isNonempty),
         ("-t",isTerm),("-u",isSetUid),("-w",isWritable),
         ("-x",isExecutable),("-z",r null),("-L",isSymlink),
         ("-O",isOwner),("-G",isGroup),("-S",isSocket)]
    where r f a = return $ f a

newerThan, sameFile :: String -> String -> IO Bool
newerThan a b = do ta <- getModificationTime a
                   tb <- getModificationTime b
                   return (ta > tb)
                `catch` \_ -> return False

sameFile a b = do a' <- canonicalizePath a
                  b' <- canonicalizePath b
                  return (a' == b')
               `catch` \_ -> return False

isFipo, isTerm, isNonempty, isReadable, isRegularFile,
    isSocket, isExecutable, isSetGid, isSetUid, isOwner, isGroup,
    isSymlink, isWritable, isSticky, isCharFile, isBlockFile :: String -> IO Bool
isBlockFile _ = return False
isCharFile _ = return False
isSticky _ = return False
isSymlink _ = return False
isGroup _ = return False
isOwner _ = return False
isSetUid _ = return False
isSetGid _ = return False
isSocket _ = return False
isTerm _ = return False
isFipo _ = return False

isWritable f = (writable `fmap` getPermissions f) `catch` \_ -> return False
isExecutable f = (do p <- getPermissions f
                     return (executable p || searchable p)) `catch` \_ -> return False
isReadable f = (readable `fmap` getPermissions f) `catch` \_ -> return False
isRegularFile = doesFileExist
isNonempty f = do h <- openFile f ReadMode
                  l <- hFileSize h
                  hClose h
                  return $ l > 0
               `catch` \_ -> return False

{-
    -b file       True if file exists and is a block special file.
    -c file       True if file exists and is a character special file.
    -d file       True if file exists and is a directory.
    -e file       True if file exists (regardless of type).
    -f file       True if file exists and is a regular file.
    -g file       True if file exists and its set group ID flag is set.
    -h file       True if file exists and is a symbolic link.
    -k file       True if file exists and its sticky bit is set.
    -n string     True if the length of string is nonzero.
    -p file       True if file is a named pipe (FIFO).
    -r file       True if file exists and is readable.
    -s file       True if file exists and has a size greater than zero.
    -t file_descriptor
                  True if the file whose file descriptor number is
                  file_descriptor is open and is associated with a terminal.
    -u file       True if file exists and its set user ID flag is set.
    -w file       True if file exists and is writable.  True indicates only
                  that the write flag is on.  The file is not writable on a
                  read-only file system even if this test indicates true.
    -x file       True if file exists and is executable.  True indicates only
                  that the execute flag is on.  If file is a directory, true
                  indicates that file can be searched.
    -z string     True if the length of string is zero.
    -L file       True if file exists and is a symbolic link.  This operator
                  is retained for compatibility with previous versions of this
                  program.  Do not rely on its existence; use -h instead.
    -O file       True if file exists and its owner matches the effective user
                  id of this process.
    -G file       True if file exists and its group matches the effective group
                  id of this process.
    -S file       True if file exists and is a socket.
-}

-- This is a very tricky function!
{-

 [ ( ( ) ] -> true
 [ ( ) ) ] -> true
 parse a single expression inside the paren...

 [ -e = -e ]

 Rules:
  1. if we ever see a binary operator with a string on each side, we
     instantly assume it's a binary operator.
  2. if next char is a unary operator or a paren, AND there's still space,
     parse that
  3. when parsing parens, if end is never found, treat as regular string
  4. 3-stage process for -a and -o
  5. -a is faster than -o
-}
