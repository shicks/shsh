\chapter{Builtins.Mkdir module}

\begin{code}

module System.Console.ShSh.Builtins.Mkdir ( mkDir ) where

import System.Console.ShSh.Shell ( Shell, ShellT, withSubStateCalled, (.~) )

import Control.Monad ( when )
import Control.Monad.State ( get, modify )
import Control.Monad.Trans ( liftIO )
import Data.List ( intercalate )
import System.Console.GetOpt
import System.Directory ( getCurrentDirectory, doesDirectoryExist,
                          setCurrentDirectory, createDirectory )
import System.FilePath ( splitDirectories, joinPath )
import System.Exit ( ExitCode(..) )

data Opts = Opts { mode :: String,
                   parents :: Bool,
                   verbose :: Bool
                 }
noOpts = Opts { mode = "644", parents = False, verbose = False }

optSpec = [Option "m" ["mode"]
             (ReqArg .~ "MODE" $ \s -> modify (\o -> o { mode = s }))
             "set file mode (as in chmod), not a=rwx - umask",
           Option "p" ["parents"]
             (NoArg $ modify $ \o -> o { parents = True })
             "no error if existing, make parent directories as needed",
           Option "v" ["verbose"]
             (NoArg $ modify $ \o -> o { verbose = True })
             "print a message for each created directory",
           Option "h" ["help"] (NoArg $ usage >> fail "")
             "display this help and exit",
           Option "V" ["version"] (NoArg $ version >> fail "")
             "output version information and exit"
          ]

-- make this into bversion :: String -> ..., defined elsewhere
version = liftIO $ putStrLn $ "mkdir (ShSh builtin)" -- version number?
usage = liftIO $ putStrLn $ usageInfo header optSpec
    where header = "Usage: mkdir [OPTION] DIRECTORY...\n"++
                   "Create the DIRECTORY(ies), if they do not already exist."
                   -- report bugs to ...?

-- We don't strictly need a substate here, but it doesn't really hurt much
mkDir :: [String] -> Shell ExitCode
mkDir args = do withSubStateCalled "mkdir" (sequence_ opts>>run) noOpts
                return ExitSuccess
    where (opts,dirs,errs) = getOpt Permute optSpec args
          run :: ShellT Opts ()
          run = do when (not $ null errs) $ fail $ intercalate ", " errs
                   mapM_ mk dirs -- would like to do these in parallel...
          mk d = do let die e = fail $ "cannot create directory `"++d++"': "++e
                    Opts m p v <- get
                    dex <- liftIO $ doesDirectoryExist d
                    when dex $ die "File exists"
                    -- what about root...?
                    let parent = joinPath $ reverse $ drop 1 $
                                 reverse $ splitDirectories d
                    pex <- liftIO $ doesDirectoryExist parent
                    when (not pex) $ if p 
                                     then mk parent 
                                     else die "No such file or directory"
                    liftIO $ createDirectory d -- should catch here ...
                    when v $ liftIO $ putStrLn $
                             "mkdir: created directory `"++d++"'"

\end{code}