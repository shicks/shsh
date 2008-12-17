module System.Console.ShSh.Builtins.Cmp ( cmp ) where

import System.Console.ShSh.IO ( iGetContents, oPutStrLn, ePutStrLn )
import System.Console.ShSh.Shell ( Shell, ShellT, withSubState )
import System.Console.ShSh.ShellError ( exit )

import Control.Monad ( when, unless )
import Control.Monad.State ( gets, modify )
import Control.Monad.Trans ( liftIO )
import System.Console.GetOpt
import System.Directory ( copyFile, doesFileExist )
import System.Exit ( ExitCode(..) )

data Opts = Opts { verbose :: Bool }
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
version = oPutStrLn $ "cmp (ShSh builtin)" -- version number?
usage = oPutStrLn $ usageInfo header optSpec
    where header = "Usage: cmp FILE1 FILE2\n"++
                   "Compare two files byte by byte."

-- We don't strictly need a substate here, but it doesn't really hurt much
cmp :: [String] -> Shell ExitCode
cmp args = do withSubState (sequence_ opts>>run) noOpts
              return ExitSuccess
    where (opts,fs,errs) = getOpt Permute optSpec args
          run :: ShellT Opts ()
          run = do when (not $ null errs) $ fail $ unlines errs
                   case fs of
                     [] -> fail "missing operand"
                     [s] -> fail $ "missing operand after `"++s++"'"
                     [a,b] -> do --oPutStrLn $ unwords ["running cmp",a,b]
                                 aa <- readF a
                                 bb <- readF b
                                 if length aa == length bb && aa == bb
                                    then return ()
                                    else fail $ unwords ["files",a,"and",b,
                                                         "differ.",
                                                         show $ length aa,
                                                         show $ length bb]
                     args -> fail $ "too many arguments:  "++ show (length args)
          readF "-" = filter (/='\r') `fmap` iGetContents
          readF f = do exists <- liftIO $ doesFileExist f
                       unless exists $ fail $ unwords [f,"does not exist"]
                       filter (/='\r') `fmap` liftIO (readFile f)
