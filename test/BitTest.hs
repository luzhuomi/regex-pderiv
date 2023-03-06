module Main where

import System.Exit (exitFailure)

import Text.Regex.PDeriv.BitCode.Transduce
import Text.Regex.PDeriv.IntPattern
import Text.Regex.PDeriv.Common


main = do
    let r4 = strip p4
        trans = buildTrans r4
        regex = buildRegex p4
    putStrLn (show $ parseRegex regex "ABAAC" == parseBX trans r4 "ABAAC")
