> module Main where

> import Text.Regex.PDeriv.BitCode.Transduce  -- AKA RightToLeft
> import Data.Maybe
> import qualified Data.ByteString.Char8 as S

> usPat = S.pack "^(.*) ([A-Za-z]{2}) ([0-9]{5})(-[0-9]{4})?$"

usPat = S.pack "^([A-Za-z0-9 \\-\\.]*) ([A-Za-z]{2}) ([0-9]{5})(-[0-9]{4})?$"

> addr = S.pack "Mountain View, CA 90410"

> parseUSAddrCompiled compiled s = case regexec compiled s of 
>                                  (Right (Just (_,_,_,l))) -> Just l
>                                  _ -> Nothing


> main :: IO ()
> main = do { f <- {-# SCC "myread" #-} S.readFile "/tmp/addr.txt"
>           ; let ls = S.lines f
>                 compiled = case compile usPat of
>                            Left _ -> error " compilation failed . "
>                            Right r -> r
>                 results = (map (parseUSAddrCompiled compiled) ls)
>           ; {-# SCC "mywrite" #-}  putStrLn  $ show results
>           -- ; putStrLn $ show (length (filter isJust results))
>           }


