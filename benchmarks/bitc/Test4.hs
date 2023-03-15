module Main where

-- Two-Pass Greedy Regular Expression Parsing  Fig 5 (b)

import System.Environment (getArgs)
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
    [n] <- getArgs
    f <- {-# SCC "myread" #-} S.readFile "./input.txt"
    let ls = S.lines f
        pat = mkpat (read n)
        compiled = case compile pat of
            Left _ -> error " compilation failed . "
            Right r -> r
        results = (map (parseCompiled compiled) ls)
    {-# SCC "mywrite" #-} putStrLn $ show results
    -- ; putStrLn $ show (length (filter isJust results))
