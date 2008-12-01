module System.Console.ShSh.Builtins.Mv ( mv ) where

import System.Console.ShSh.IO ( oPutStrLn, ePutStrLn )
import System.Console.ShSh.Shell ( Shell, ShellT, withSubState )
import System.Console.ShSh.ShellError ( exit )

import System.FilePath ( (</>), takeFileName )
import Control.Monad ( when )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Console.GetOpt
import System.Directory ( renameFile, doesDirectoryExist )
import System.Exit ( ExitCode(..) )

data Opts = Opts { verbose :: Bool }
noOpts :: Opts
noOpts = Opts { verbose = False }

optSpec = [Option "v" ["verbose"]
             (NoArg $ modify $ \o -> o { verbose = True })
             "print a message for each file copied",
           Option "h" ["help"] (NoArg $ usage >> exit 0)
             "display this help and exit",
           Option "V" ["version"] (NoArg $ version >> exit 0)
             "output version information and exit"
          ]

-- make this into bversion :: String -> ..., defined elsewhere
version = oPutStrLn $ "mv (ShSh builtin)" -- version number?
usage = oPutStrLn $ usageInfo header optSpec
    where header = "Usage: mv [OPTION]... SOURCE DEST\n"++
                   "  or:  mv [OPTION]... SOURCE... DIRECTORY\n"++
                   "Rename SOURCE to DEST, or move SOURCE(s) to DIRECTORY."

-- We don't strictly need a substate here, but it doesn't really hurt much
mv :: [String] -> Shell ExitCode
mv args = do withSubState (sequence_ opts>>run) noOpts
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
                               [s,_] | not isdir && last dest /= '/' -> mv1 s dest
                               _ -> do when (not isdir) $
                                            fail $ "target `"++dest++"' is not a directory."
                                       mapM_ (\s -> mv1 s (dest </> takeFileName s)) $ init fs
          mv1 s d = do amV <- gets verbose
                       when amV $ ePutStrLn $ unwords ["mv",s,d]
                       liftIO $ renameFile s d
