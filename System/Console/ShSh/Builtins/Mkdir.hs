{-# OPTIONS_GHC -cpp #-}
module System.Console.ShSh.Builtins.Mkdir ( mkDir ) where

import System.Console.ShSh.Directory ( parentDir )
import System.Console.ShSh.IO ( oPutStrLn, ePutStrLn )
import System.Console.ShSh.Shell ( Shell, ShellT, withSubState )
import System.Console.ShSh.ShellError ( exit )

import Control.Monad ( when )
import Control.Monad.State ( get, modify )
import Control.Monad.Trans ( liftIO )
import System.Console.GetOpt
import System.Directory ( getCurrentDirectory, doesDirectoryExist,
                          setCurrentDirectory, createDirectory )
import System.Exit ( ExitCode(..) )

#ifdef UNIX
import System.Posix.Files ( setFileMode )
-- import System.Console.ShSh.Unix ( setFileMode, fileMode )
#endif

data Opts = Opts { mode :: String,
                   parents :: Bool,
                   verbose :: Bool
                 }
noOpts = Opts { mode = "644", parents = False, verbose = False }

optSpec = [Option "m" ["mode"]
             (ReqArg `flip` "MODE" $ \s -> modify (\o -> o { mode = s }))
             "set file mode (as in chmod), not a=rwx - umask",
           Option "p" ["parents"]
             (NoArg $ modify $ \o -> o { parents = True })
             "no error if existing, make parent directories as needed",
           Option "v" ["verbose"]
             (NoArg $ modify $ \o -> o { verbose = True })
             "print a message for each created directory",
           Option "h" ["help"] (NoArg $ usage >> exit 0)
             "display this help and exit",
           Option "V" ["version"] (NoArg $ version >> exit 0)
             "output version information and exit"
          ]

-- make this into bversion :: String -> ..., defined elsewhere
version = oPutStrLn $ "mkdir (ShSh builtin)" -- version number?
usage = oPutStrLn $ usageInfo header optSpec
    where header = "Usage: mkdir [OPTION] DIRECTORY...\n"++
                   "Create the DIRECTORY(ies), if they do not already exist."
                   -- report bugs to ...?

-- We don't strictly need a substate here, but it doesn't really hurt much
mkDir :: [String] -> Shell ExitCode
mkDir args = do withSubState (sequence_ opts>>run) noOpts
                return ExitSuccess
    where (opts,dirs,errs) = getOpt Permute optSpec args
          run :: ShellT Opts ()
          run = do when (not $ null errs) $ fail $ unlines errs
                   mapM_ mk dirs -- would like to do these in parallel...
          mk d = do let die e = fail $ "cannot create directory `"++d++"': "++e
                    Opts m p v <- get
                    dex <- liftIO $ doesDirectoryExist d
                    if dex
                        then if p
                             then return () -- okay w/ -p
                             else die "File exists"
                        else
                          do case parentDir d of
                               "" -> return () -- infinite loop on relative dirs
                               "/" -> return () -- no point trying to mkdir /
                               parent -> do pex <- liftIO $
                                                   doesDirectoryExist parent
                                            when (not pex) $ if p
                                                             then mk parent
                                                             else die nsfd
                             liftIO $ createDirectory d -- should catch here ...
#ifdef UNIX 
                             -- check for windows here and warn/ignore?
                             liftIO $ setFileMode d (fileMode m)
#endif
                             when v $ ePutStrLn $ created d
          nsfd = "No such file or directory"
          created d = "mkdir: created directory `"++d++"'"
