module Main where

-- Two-Pass Greedy Regular Expression Parsing  Fig 4 (a)
import qualified Data.ByteString.Char8 as S
import Data.Maybe
import Text.Regex.PDeriv.BitCode.Transduce

pat = S.pack "^(a*)$"


as = S.pack "aaaaaaaaaaaaaaaaaaaa"

parseCompiled compiled s = case regexec compiled s of
  (Right (Just (_, _, _, l))) -> Just l
  _ -> Nothing

main :: IO ()
main = do
  f <- {-# SCC "myread" #-} S.readFile "./input.txt"
  let ls = S.lines f
      compiled = case compile pat of
        Left _ -> error " compilation failed . "
        Right r -> r
      results = (map (parseCompiled compiled) ls)
  {-# SCC "mywrite" #-} putStrLn $ show results
  -- ; putStrLn $ show (length (filter isJust results))
