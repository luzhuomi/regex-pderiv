module Main where

import System.Exit (exitFailure)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Text.Regex.PDeriv.BitCode.Transduce
import Text.Regex.PDeriv.IntPattern
import Text.Regex.PDeriv.Common


main = defaultMain unitTests


unitTests =
  testGroup
    "Unit tests" [testBitCode]

testBitCode = 
    let r4 = strip p4
        trans = buildTrans r4
        regex = buildRegex p4
    in testCase "parseRegex should be the same as parseBX" $ assertEqual [] (parseRegex regex "ABAAC") (parseBX trans r4 "ABAAC")


