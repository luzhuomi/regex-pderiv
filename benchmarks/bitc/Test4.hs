module Main where

-- Two-Pass Greedy Regular Expression Parsing  Fig 5 (b)

import System.Environment (getArgs)
import Data.Time.Clock.System
import qualified Data.ByteString.Char8 as S
import Data.Maybe
import Text.Regex.PDeriv.BitCode.Transduce

mkpat :: Int -> S.ByteString
mkpat n = 
    let aqs = S.intercalate S.empty $ take n $ repeat (S.pack "(a?)")
        as = S.pack $ take n $ repeat 'a'
    in (S.pack "^") `S.append` as `S.append` aqs `S.append` (S.pack "$")


-- pat = S.pack "^(([ab])*a[ab][ab])$"

-- as = S.pack "ababab"



parseCompiled compiled s = case regexec compiled s of
  (Right (Just (_, _, _, l))) -> Just l
  _ -> Nothing

main :: IO ()
main = do
    [n, fname] <- getArgs
    f <- {-# SCC "myread" #-} S.readFile fname
    t0 <- getSystemTime
    let ls = S.lines f
        pat = mkpat (read n)
        compiled = case compile pat of
            Left _ -> error " compilation failed . "
            Right r -> r
    putStrLn $ show $ numTrans compiled 
    t1 <- compiled `seq` getSystemTime
    putStrLn (show ((systemNanoseconds t1 - systemNanoseconds t0) `div` 1000) ++ " micro sec")
    let results = (map (parseCompiled compiled) ls)
    putStrLn $ show results
    t2 <- getSystemTime
    putStrLn (show ((systemNanoseconds t2 - systemNanoseconds t1) `div` 1000) ++ " micro sec")
    -- ; putStrLn $ show (length (filter isJust results))
