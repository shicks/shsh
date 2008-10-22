\chapter{ShellError module}

We'll isolate some of this stuff, since it's pretty independent.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module System.Console.ShSh.ShellError ( ShellError(..), isFailure,
                                        exit, exitCode,
                                        rethrow, catchS,
                                        announceError,
                                        prefixError, withPrefix
                                      ) where

import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.Error ( MonadError, throwError, catchError,
                             Error, strMsg )
import System.Exit ( ExitCode(..) )

data ShellError = ShellError ExitCode String
instance Error ShellError where
    strMsg s = ShellError (ExitFailure 1) s
instance Show ShellError where
    show (ShellError _ s) = s

prefixError :: String -> ShellError -> ShellError
prefixError _ (ShellError e "") = ShellError e ""
prefixError "" (ShellError e s) = ShellError e s
prefixError s (ShellError e s') = ShellError e $ s++": "++s'

-- Catch success exceptions, rethrow failures.
catchS :: MonadError ShellError m =>
          m a -> (String -> m a) -> m a
catchS a s = catchError a $ \e -> rethrow e >> s (show e)

isFailure :: ShellError -> Bool
isFailure (ShellError ExitSuccess _) = False
isFailure _ = True

rethrow :: MonadError ShellError m => ShellError -> m ()
rethrow (ShellError ExitSuccess _) = return ()
rethrow e = throwError e

-- Generally, either fail or exit will "do the right thing"...
exit :: MonadError ShellError m => Int -> m a
exit 0 = throwError $ ShellError ExitSuccess ""
exit n = throwError $ ShellError (ExitFailure n) ""

exitCode :: ShellError -> ExitCode
exitCode (ShellError e _) = e

announceError :: MonadIO m => ShellError -> m ()
announceError (ShellError _ "") = return ()
announceError e = liftIO $ putStrLn $ show $ prefixError "shsh" e

-- This is a bit silly - we just rethrow the errors with a prefix...
withPrefix :: MonadError ShellError m => String -> m a -> m a
withPrefix h s = catchError s (\e -> throwError $ prefixError h e)

\end{code}