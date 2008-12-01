module System.Console.ShSh.Builtins.Cp ( cp ) where

import System.Console.ShSh.IO ( oPutStrLn, ePutStrLn )
import System.Console.ShSh.Shell ( Shell, ShellT, withSubState )
import System.Console.ShSh.ShellError ( exit )

import Data.List ( delete )
import System.FilePath ( (</>), takeFileName )
import Control.Monad ( when )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Console.GetOpt
import System.Directory ( copyFile, doesDirectoryExist, createDirectory,
                          getDirectoryContents, getPermissions, setPermissions )
import System.Exit ( ExitCode(..) )

data Opts = Opts { recursive :: Bool,
                   verbose :: Bool }
noOpts = Opts { recursive = False, verbose = False }

optSpec = [Option "rR" ["recursive"]
             (NoArg $ modify $ \o -> o { recursive = True })
             "copy directories recursively",
           Option "v" ["verbose"]
             (NoArg $ modify $ \o -> o { verbose = True })
             "print a message for each file copied",
           Option "h" ["help"] (NoArg $ usage >> exit 0)
             "display this help and exit",
           Option "V" ["version"] (NoArg $ version >> exit 0)
             "output version information and exit"
          ]

-- make this into bversion :: String -> ..., defined elsewhere
version = oPutStrLn $ "cp (ShSh builtin)" -- version number?
usage = oPutStrLn $ usageInfo header optSpec
    where header = "Usage: cp [OPTION]... SOURCE DEST\n"++
                   "  or:  cp [OPTION]... SOURCE... DIRECTORY\n"++
                   "Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY."

-- We don't strictly need a substate here, but it doesn't really hurt much
cp :: [String] -> Shell ExitCode
cp args = do withSubState (sequence_ opts>>run) noOpts
             return ExitSuccess
    where (opts,fs,errs) = getOpt Permute optSpec args
          run :: ShellT Opts ()
          run = do when (not $ null errs) $ fail $ unlines errs
                   case fs of
                     [] -> fail "missing operand"
                     [s] -> fail $ "missing destination operand after `"++s++"'"
                     _ -> do let dest = last fs
                             isdir <- liftIO $ doesDirectoryExist dest
                             case fs of
                               [s,_] | not isdir && last dest /= '/' -> cp1 s dest
                               _ -> do when (not isdir) $
                                            fail $ "target `"++dest++"' is not a directory."
                                       mapM_ (\s -> cp1 s (dest </> takeFileName s)) $ init fs
          cp1 s d = do amV <- gets verbose
                       when amV $ ePutStrLn $ unwords ["cp",s,d]
                       amR <- gets recursive
                       isdir <- if amR then liftIO $ doesDirectoryExist s
                                       else return False
                       if isdir
                          then do liftIO $ do createDirectory d
                                              setPermissions d =<< getPermissions s
                                  subfs <- liftIO $ getDirectoryContents s
                                  mapM_ (\ss -> cp1 (s</>ss) (d</>ss)) $ delete "." $ delete ".." subfs
                          else liftIO $ copyFile s d
