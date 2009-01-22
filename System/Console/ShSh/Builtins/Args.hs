{-# OPTIONS_GHC -Wall #-}

-- |This sets up a boilerplate withArgs modifier to allow simple
-- builtins (like echo) to take simple args.
module System.Console.ShSh.Builtins.Args ( withArgs, withArgsOrd, ShellA,
                                           flagOn, flagOff, optSet,
                                           opt, flag ) where

import Control.Monad ( unless )
import Control.Monad.State ( get, modify )
import Data.Maybe ( fromJust )
import Data.List ( (\\) )

import System.Console.GetOpt ( ArgOrder(..), OptDescr(..), ArgDescr(..),
                               getOpt, usageInfo )

import System.Console.ShSh.IO ( oPutStrLn )
import System.Console.ShSh.Shell ( Shell, ShellT,
                                   withSubState, withErrorsPrefixed )
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
helpVers name header optSpec = optSpec' (concatMap short optSpec)
                                        (concatMap long optSpec)
    where optSpec' ss ls = optSpec ++ filter valid
                                        [Option h help
                                         (NoArg $ usage >> exit 0)
                                         "display this help and exit"
                                        ,Option v version
                                         (NoArg $ vers >> exit 0)
                                         "output version information and exit"]
              where h = if 'h' `elem` ss then "" else "h"
                    v = if 'V' `elem` ss then "" else "V"
                    help = if "help" `elem` ls then [] else ["help"] 
                    version = if "version" `elem` ls then [] else ["version"]
                    valid (Option s l _ _) = not (null s) || not (null l)
          vers = oPutStrLn $ name ++ " (ShSh builtin)" -- version number?
          usage = oPutStrLn $ usageInfo header optSpec
          short (Option s _ _ _) = s
          long (Option _ l _ _) = l

-- |Sets up a substate in a ShellA in which the args are parsed and can
-- be accessed by the flag and opt functions.
withArgs :: Eq a      -- ^key type (usually 'Char')
         => String    -- ^name of job - used for error messages, etc.
         -> String    -- ^header for usage
         -> [Arg a]   -- ^argspec
         -> ([String] -> ShellA a x) -- ^job to do, passed unparsed args
         -> [String] -> Shell x
withArgs = withArgs' Permute

-- |Same as above, but required options to come before args.
withArgsOrd :: Eq a      -- ^key type (usually 'Char')
            => String    -- ^name of job - used for error messages, etc.
            -> String    -- ^header for usage
            -> [Arg a]   -- ^argspec
            -> ([String] -> ShellA a x) -- ^job to do, passed unparsed args
            -> [String] -> Shell x
withArgsOrd = withArgs' RequireOrder

-- |This is the internal version
withArgs' :: Eq a => ArgOrder (ShellA a ()) -> String -> String -> [Arg a]
                  -> ([String] -> ShellA a x) -> [String] -> Shell x
withArgs' ord name header spec job args = withErrorsPrefixed name $
                                          withSubState `flip` [] $
                                          sequence_ opts >> run
    where (opts,rest,errs) = getOpt ord (helpVers name header spec) args
          run = do unless (null errs) $ fail $ unlines errs
                   job rest


flag :: Eq a => a -> ShellA a Bool
flag key = (maybe False (const True) . lookup key) `fmap` get

opt :: Eq a => a -> String -> ShellA a String
opt key def = (maybe def fromJust . lookup key) `fmap` get

-- Useful construct: return unless `ap` flag 'n' `ap` oPutStrLn ""
