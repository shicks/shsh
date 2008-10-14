\chapter{Parse module}

Here we'll have code to take an input string and parse it into
a command, including dealing with pipes, redirection, etc.

\begin{code}

module Parse where

import Shell ( Shell )

-- The type of parse is String -> Maybe (Shell ()).  This is so
-- that we can "send an input back" if it's not complete, i.e.
--   $ echo "abc
--   > def"
-- where the second prompt was used to wait for termination of the
-- quoted string.  In this case, parse will return Nothing, so that
-- the event loop can put up another prompt and then resend the
-- input to parse after the next line of input.
parse :: String -> Maybe (Shell ())

tokenize :: String -> Maybe [String]
tokenize = tok' []

-- tok' is a helper function that takes a stack of the current reading
-- environment, i.e. are we inside a quote or backtick or paren?
-- This could probably be done with a Stack monad, but we'll try without.
-- Actually... this "Maybe []" construct already has some monadic
-- semantics: we'd like Maybe [x] ++ Maybe [y] -> Maybe (x++y), but
-- Maybe [x] ++ Nothing -> Nothing... is there already something that
-- does this?
tok' :: [Char] -> String -> Maybe [String]
tok' context 

\end{code}