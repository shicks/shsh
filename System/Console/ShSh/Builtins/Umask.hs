{-# OPTIONS_GHC -Wall -fffi -cpp #-}
module System.Console.ShSh.Builtins.Umask ( umask ) where

import System.Console.ShSh.Shell ( Shell )

import Control.Monad ( guard )
import Control.Monad.Trans ( liftIO )
import Foreign.C.Types ( CInt )
import Data.Char ( ord )
import System.Exit ( ExitCode(..) )

{-# NOINLINE umask #-}
umask :: [String] -> Shell ExitCode
umask [mode] =
    case fromOctal mode of
      Nothing -> fail "umask requires an octal mode"
      Just m -> do liftIO $ c_umask m
                   return ExitSuccess
umask _ = fail "umask requires a single argument!"

#ifdef HAVE_UMASK
foreign import ccall unsafe "static sys/stat.h umask" c_umask :: CInt -> IO CInt
#else
c_umask :: CInt -> IO CInt
c_umask = return
#endif

fromOctal :: String -> Maybe CInt
fromOctal s = fo 0 s
    where fo x "" = Just $ fromIntegral x
          fo x (c:cs) = do let cval = ord c - ord '0'
                           guard $ cval <= 7 && cval >= 0
                           fo (8*x + cval) cs
