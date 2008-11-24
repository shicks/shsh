-- |This is now basically just a stub to interface with
-- the 'Language.Sh.Expansion' module.

module System.Console.ShSh.Expansion ( expandWord, expandWords ) where

import Language.Sh.Syntax ( Word )
import qualified Language.Sh.Expansion as E
import System.Console.ShSh.Foreign.Pwd ( getHomeDir )
import System.Console.ShSh.Shell ( Shell, setEnv, getEnv )

import Control.Monad.Trans ( liftIO )

-- |Functions to pass to @Expansion@ module.
ef :: E.ExpansionFunctions Shell
ef = E.ExpansionFunctions { E.getEnv = getEnv,
                            E.setEnv = setEnv,
                            E.homeDir = liftIO . getHomeDir,
                            E.expandGlob = E.noGlobExpansion,
                            E.runCommands = const $ return "" }

-- |Expand a single word into a single string; no field splitting
expandWord :: Word -> Shell String
expandWord = E.expandWord ef

-- |Expand a list of words into a list of strings; fields will be split.
expandWords :: [Word] -> Shell [String]
expandWords = E.expand ef
