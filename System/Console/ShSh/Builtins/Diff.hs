{-# OPTIONS_GHC -Wall #-}
module System.Console.ShSh.Builtins.Diff ( diff ) where

import System.Console.ShSh.IO ( oPutStr, oPutStrLn )
import System.Console.ShSh.Shell ( Shell )
import System.Console.ShSh.Builtins.Args ( withArgs, flag, flagOn )
import System.Console.ShSh.Builtins.Util ( readFileOrStdin )

import qualified Data.Set as S
import Data.List ( sort )
import Data.Array.ST
import Control.Monad.ST

import Control.Monad.Trans ( liftIO )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.Exit ( ExitCode(..) )

{-# NOINLINE diff #-}
diff :: [String] -> Shell ExitCode
diff = withArgs "diff" header args diff'
    where diff' [old,new] =
              do amdiff <- sdiff old new
                 if amdiff then return $ ExitFailure 1
                           else return ExitSuccess
                 
          diff' _ = fail "diff requires two file arguments!"
          header = "Usage: diff [options...] oldfile newfile\n"++
                   "compare files line by line"
          args = [flagOn "i" ["ignore-case"] 'i'
                             "ignore case differences in file contents",
                  flagOn "uc" ["unified"]   'u' "unidiff output",
                  flagOn "r" ["recursive"] 'r' "diff recursively"]
          sdiff old new =
              do optRecursive <- flag 'r'
                 odir <- liftIO $ doesDirectoryExist old
                 ndir <- liftIO $ doesDirectoryExist new
                 case (odir,ndir) of
                   (False, False) -> do o <- lines `fmap` readFileOrStdin old
                                        n <- lines `fmap` readFileOrStdin new
                                        oPutStrLn $ "--- "++old
                                        oPutStrLn $ "+++ "++new
                                        diffIt o n
                                        return (o /= n)
                   (True, False) -> do oPutStrLn $ "Only old directory "++old
                                       return True
                   (False, True) -> do oPutStrLn $ "Only new directory "++new
                                       return True
                   (True, True) -> if optRecursive
                                   then do conts <- filter (`notElem` [".",".."])
                                                    `fmap`
                                                    liftIO (getDirectoryContents old)
                                           amd <- mapM (\x -> sdiff (old++"/"++x)
                                                                    (new++"/"++x))
                                                  conts
                                           return $ or amd
                                   else return False
          diffIt o n = mkdiff 1 1 [] (lazyLcs o n) o n
          mkdiff nx ny ctx (l:ls) (x:xs) (y:ys)
                 | l == x && l == y = mkdiff (nx+1) (ny+1) (l:ctx) ls xs ys
          mkdiff nx ny ctx (l:ls) xs ys =
              do let rmd = takeWhile (/= l) xs
                     add = takeWhile (/= l) ys
                     restx = drop (length rmd + 1) xs
                     resty = drop (length add + 1) ys
                     npostctx = if take 2 restx == take 2 resty &&
                                   take 2 restx == take 2 ls
                                then 3
                                else if take 1 restx == take 1 resty &&
                                        take 1 restx == take 1 ls
                                     then 2
                                     else 1
                 oPutStrLn $ showHunk nx ny (length $ take 3 ctx)
                                     (length rmd) (length add) npostctx
                 oPutStr $ unlines $ showContext (reverse $ take 3 ctx)
                 oPutStr $ unlines $ showRm rmd
                 oPutStr $ unlines $ showAdd add
                 oPutStr $ unlines $ showContext $ take npostctx (l:ls)
                 mkdiff (nx+length rmd+npostctx) (ny+length add+npostctx) []
                        (drop (npostctx-1) ls) (drop (npostctx-1) restx)
                                               (drop (npostctx-1) resty)
          mkdiff _ _ _ [] [] [] = return ()
          mkdiff nx ny ctx [] xs ys =
              do oPutStrLn $ showHunk nx ny (length $ take 3 ctx)
                              (length xs) (length ys) 0
                 oPutStr $ unlines $ showContext (reverse $ take 3 ctx)
                 oPutStr $ unlines $ showRm xs
                 oPutStr $ unlines $ showAdd ys
          showHunk nx ny nctx nrm nadd npost =
              "@@ -"++show (nx-nctx)++","++show (nctx+nrm+npost)
              ++" +"++show (ny-nctx)++","++show (nctx+nadd+npost)++" @@"
          showRm :: [String] -> [String]
          showRm = map ('-':)
          showAdd :: [String] -> [String]
          showAdd = map ('+':)
          showContext :: [String] -> [String]
          showContext = map (' ':)

lcs :: Ord a => [a] -> [a] -> [a]

findUnique :: Ord a => [a] -> S.Set a
findUnique xs = fu S.empty S.empty xs
    where fu _ uni [] = uni
          fu multi uni (y:ys)
              | y `S.member` uni = fu (S.insert y multi) (S.delete y uni) ys
              | y `S.member` multi = fu multi uni ys
              | otherwise = fu multi (S.insert y uni) ys

findDoubleUnique :: Ord a => [a] -> [a] -> S.Set a
findDoubleUnique xs ys = S.intersection (findUnique xs) (findUnique ys)

lazyLcs :: Ord a => [a] -> [a] -> [a]
lazyLcs [] _ = []
lazyLcs _ [] = []
lazyLcs xs ys
    | S.null du = lcs xs ys
    | otherwise = case (filter (`S.member` du) xs, filter (`S.member` du) ys) of
                  (xs',ys') -> case lcs xs' ys' of
                               l' -> joinU l' xs ys
    where du = findDoubleUnique xs ys
          joinU [] x y = lazyLcs x y
          joinU (b:bs) cs ds = lazyLcs c1 d1 ++ b : joinU bs c2 d2
              where (c1,_:c2) = break (==b) cs
                    (d1,_:d2) = break (==b) ds

lcs [] _ = []
lcs _ [] = []
lcs (c1:c1s) (c2:c2s)
    | c1 == c2 = c1: lcs c1s c2s
    | otherwise =
        reverse $ lcs_simple (reverse (c1:c1s)) (reverse (c2:c2s))

lcs_simple :: Ord a => [a] -> [a] -> [a]
lcs_simple [] _ = []
lcs_simple _ [] = []
lcs_simple s1@(c1:c1s) s2@(c2:c2s)
    | c1 == c2 = c1: lcs c1s c2s
    | otherwise =
        case unzip $ prune_matches s1 $! find_matches s1 s2 of
        (s1',m1') -> hunt s1' m1'

prune_matches :: [a] -> [[Int]] -> [(a, [Int])]
prune_matches _ [] = []
prune_matches [] _ = []
prune_matches (_:cs) ([]:ms) = prune_matches cs ms
prune_matches (c:cs) (m:ms) = (c,m): prune_matches cs ms

type Threshold s a = STArray s Int (Int,[a])

hunt :: [a] -> [[Int]] -> [a]
hunt [] _ = []
hunt cs matches =
    runST (do
           th <- empty_threshold (length cs) l
           hunt_internal cs matches th
           hunt_recover th (-1) l
          )
    where l = foldl max 0 $ concat matches

hunt_internal :: [a] -> [[Int]] -> Threshold s a ->
                 ST s ()
hunt_internal [] _ _ = return ()
hunt_internal _ [] _ = return ()
hunt_internal (c:cs) (m:ms) th = do
    hunt_one_char c m th
    hunt_internal cs ms th

hunt_one_char :: a -> [Int] ->  Threshold s a ->
                 ST s ()
hunt_one_char _ [] _ = return ()
hunt_one_char c (j:js) th = do
    index_k <- my_bs j th
    case index_k of
      Nothing -> return ()
      Just k -> do
        (_, rest) <- readArray th (k-1)
        writeArray th k (j, c:rest)
    hunt_one_char c js th

-- This is O(n), which is stupid.
hunt_recover :: Threshold s a -> Int -> Int -> ST s [a]
hunt_recover th n limit =
 do (_, th_max) <- getBounds th
    if n < 0
       then hunt_recover th th_max limit
       else if n == 0
            then return []
            else if n > th_max
                 then return []
                 else do (thn, sn) <- readArray th n
                         if thn <= limit
                             then return $ reverse sn
                             else hunt_recover th (n-1) limit

empty_threshold :: Int -> Int -> ST s (Threshold s a)
empty_threshold l th_max = do
  th <- newArray (0,l) (th_max+1, [])
  writeArray th 0 (0, [])
  return th

my_bs :: Int -> Threshold s a -> ST s (Maybe Int)
my_bs j th = do bnds <- getBounds th
                my_helper_bs j bnds th

my_helper_bs :: Int -> (Int,Int) -> Threshold s a ->
                ST s (Maybe Int)
my_helper_bs j (th_min,th_max) th =
    if th_max - th_min > 1 then do
       (midth, _) <- readArray th th_middle
       if j > midth
         then my_helper_bs j (th_middle,th_max) th
         else my_helper_bs j (th_min,th_middle) th
    else do
       (minth, _) <- readArray th th_min
       (maxth, _) <- readArray th th_max
       if minth < j && maxth > j
          then return $ Just th_max
          else if j < minth then return $ Just th_min
               else return Nothing
    where th_middle = (th_max+th_min) `div` 2



find_matches :: Ord a => [a] -> [a] -> [[Int]]
find_matches [] [] = []
find_matches [] (_:bs) = []: find_matches [] bs
find_matches _ [] = []
find_matches a b =
    unzip_indexed $ sort $ find_sorted_matches indexeda indexedb [] []
    where indexeda = sort $ zip a [1..]
          indexedb = sort $ zip b [1..]

unzip_indexed :: [(Int,[a])] -> [[a]]
unzip_indexed s = unzip_indexed_helper 1 s
    where unzip_indexed_helper _ [] = []
          unzip_indexed_helper thisl ((l,c):rest)
           | thisl == l = c: unzip_indexed_helper (l+1) rest
           | otherwise = []: unzip_indexed_helper (thisl+1) ((l,c):rest)

find_sorted_matches :: Ord a => [(a, Int)] -> [(a, Int)] -> [a] -> [Int]
                             -> [(Int, [Int])]
find_sorted_matches [] _ _ _ = []
find_sorted_matches _ [] _ _ = []
find_sorted_matches ((a,na):as) ((b,nb):bs) aold aoldmatches
    | [a] == aold = (na, aoldmatches) :
                  find_sorted_matches as ((b,nb):bs) aold aoldmatches
    | a > b = find_sorted_matches ((a,na):as) bs aold aoldmatches
    | a < b = find_sorted_matches as ((b,nb):bs) aold aoldmatches
-- following line is inefficient if a line is repeated many times.
    | otherwise -- a == b
      = case reverse $ find_matches_one a ((b,nb):bs) of
        matches -> (na, matches) :
                   find_sorted_matches as ((b,nb):bs) [a] matches

find_matches_one :: Eq a => a -> [(a, Int)] -> [Int]
find_matches_one _ [] = []
find_matches_one a ((b,nb):bs)
    | a == b = nb: find_matches_one a bs
    | otherwise = []
