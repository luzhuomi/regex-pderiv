module Main where

import qualified Data.ByteString.Char8 as S
import Data.Maybe
import Text.Regex.PDeriv.BitCode.Transduce


pat = S.pack "^([ab])*a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab]$"


-- pat = S.pack "^(([ab])*a[ab][ab])$"

-- as = S.pack "ababab"

parseCompiled compiled s = case regexec compiled s of
  (Right (Just (_, _, _, l))) -> Just l
  _ -> Nothing

main :: IO ()
main = do
  f <- {-# SCC "myread" #-} S.readFile "./input.txt"
  let ls = S.lines f
      -- ls = [as]
      compiled = case compile pat of
        Left _ -> error " compilation failed . "
        Right r -> r
      results = (map (parseCompiled compiled) ls)
  {-# SCC "mywrite" #-} putStrLn $ show results
  -- ; putStrLn $ show (length (filter isJust results))
