{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}

-- |This is an FFI module for dealing with the pwd.h header...
-- We'll provide reasonable alternatives in pure Haskell for those that don't
-- have the correct headers.

module System.Console.ShSh.Foreign.Pwd ( getHomeDir ) where

#ifdef HAVE_PWD
import Foreign.C.String ( CString, withCString, peekCAString )
import Foreign.Ptr ( nullPtr )
import Foreign.Marshal.Alloc ( free )

foreign import ccall unsafe "hspwd.h getHomeDir" _getHomeDir :: CString -> IO CString

getHomeDir :: String -> IO (Maybe String)
getHomeDir user = do dir <- withCString user _getHomeDir
                     if (dir==nullPtr) then return Nothing
                                       else do ret <- peekCAString dir
                                               free dir
                                               return $ Just ret
#else

getHomeDir :: String -> IO (Maybe String)
getHomeDir = const $ return Nothing -- how's that for reasonable?

#endif
