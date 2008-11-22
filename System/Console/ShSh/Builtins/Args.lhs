\chapter{Builtins.Args module}

This sets up a boilerplate withArgs modifier to allow simple
builtins (like echo) to take simple args.

\begin{code}

module System.Console.ShSh.Builtins.Args ( withArgs, ShellA,
                                           flagOn, flagOff, optSet,
                                           opt, flag ) where

import Control.Monad ( unless )
import Control.Monad.State ( get, modify )
import Data.Maybe ( fromJust )
import Data.List ( (\\) )

import System.Console.GetOpt

import System.Console.ShSh.IO ( oPutStrLn )
import System.Console.ShSh.Shell ( Shell, ShellT, withSubStateCalled )
import System.Console.ShSh.Util ( update )
import System.Console.ShSh.ShellError ( exit )

type ShellA a = ShellT [(a,Maybe String)]
type Arg a = OptDescr (ShellA a ())

flagOn :: Eq a => String -> [String] -> a -> String -> Arg a
flagOn short long key help = Option short long `flip` help $
                             NoArg $ modify $ \s -> update key Nothing s
flagOff :: Eq a => String -> [String] -> a -> String -> Arg a
flagOff short long key help = Option short long `flip` help $
                              NoArg $ modify $ \s -> s \\ [(key,Nothing)]
optSet :: Eq a => String -> [String] -> a -> String -> String -> Arg a
optSet short long key name help = Option short long `flip` help $
                                  ReqArg `flip` name $
                                  \v -> modify $ \s -> update key (Just v) s

helpVers :: String -> String -> [Arg a] -> [Arg a]
helpVers name header optSpec = optSpec'
    where optSpec' = optSpec ++ [Option "h" ["help"] (NoArg $ usage >> exit 0)
                                 "display this help and exit",
                                 Option "V" ["version"] (NoArg $ vers >> exit 0)
                                 "output version information and exit"]
          vers = oPutStrLn $ name ++ " (ShSh builtin)" -- version number?
          usage = oPutStrLn $ usageInfo header optSpec

withArgs :: Eq a
         => String  -- ^name of job - used for error messages, etc.
         -> String  -- ^header for usage
         -> [Arg a]   -- ^argspec
         -> ([String] -> ShellA a x) -- ^job to be done, passed unparsed args
         -> [String] -> Shell x
withArgs name header spec job args = withSubStateCalled name `flip` [] $
                                     sequence_ opts >> run
    where (opts,rest,errs) = getOpt Permute (helpVers name header spec) args
          run = do unless (null errs) $ fail $ unlines errs
                   job rest

flag :: Eq a => a -> ShellA a Bool
flag key = (maybe False (const True) . lookup key) `fmap` get

opt :: Eq a => a -> String -> ShellA a String
opt key def = (maybe def fromJust . lookup key) `fmap` get

-- Useful construct: return unless `ap` flag 'n' `ap` oPutStrLn ""

\end{code}