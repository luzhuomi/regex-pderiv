module Main where

import System.Exit (exitFailure)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Text.Regex.PDeriv.BitCode.Transduce
import Text.Regex.PDeriv.IntPattern
import Text.Regex.PDeriv.RE
import Text.Regex.PDeriv.Common
import Data.List (foldl')


main = defaultMain unitTests


unitTests =
  testGroup
    "Unit tests" [testBitCodeParseRegex1, testBitCodeP4, testBitCodeP5, testBitCodeP6, testBitCodeP7]



long_pat = PPair (PVar 1 [] (PE (Star (L 'A') Greedy))) (PVar 2 [] (PE (Star (L 'A') Greedy)))
long_string n = (take 0 (repeat 'A')) ++ (take n (repeat 'B'))

-- p4 = << x : (A|<A,B>), y : (<B,<A,A>>|A) >, z : (<A,C>|C) >

p4 = PPair (PPair p_x p_y) p_z
   where p_x = PVar 1 [] (PE (Choice (L 'A') (Seq (L 'A') (L 'B')) Greedy))      
         p_y = PVar 2 [] (PE (Choice (Seq (L 'B') (Seq (L 'A') (L 'A'))) (L 'A') Greedy))
         p_z = PVar 3 [] (PE (Choice (Seq (L 'A') (L 'C')) (L 'C') Greedy))

input =  "ABAAC"  -- long(posix) vs greedy match


p5 = PStar (PVar 1 [] (PE (Choice (L 'A') (Choice (L 'B') (L 'C') Greedy) Greedy))) Greedy

-- pattern = ( x :: (A|C), y :: (B|()) )*

p6 = PStar (PPair (PVar 1 [] (PE (Choice (L 'A') (L 'C') Greedy))) (PVar 2 [] (PE (Choice (L 'B') Empty Greedy)))) Greedy

-- pattern = ( x :: ( y :: A, z :: B )* )

p7 = PVar 1 [] (PStar (PPair (PVar 2 [] (PE (L 'A'))) (PVar 3 [] (PE (L 'B')))) Greedy)

input7 =  "ABABAB"


-- pattern = ( x :: A*?, y :: A*)

p8 = PPair (PVar 1 [] (PE (Star (L 'A') NotGreedy))) (PVar 2 [] (PE (Star (L 'A') Greedy)))

input8 =  "AAAAAA"

-- pattern = ( x :: A*?, y :: A*)

p9 = PPair (PStar (PVar 1 [] (PE (L 'A'))) NotGreedy) (PVar 2 [] (PE (Star (L 'A') Greedy)))

-- pattern = ( x :: (A|B)*?, (y :: (B*,A*)))

p10 = PPair (PVar 1 [] (PE (Star (Choice (L 'A') (L 'B') Greedy) NotGreedy))) (PVar 2 [] (PE (Seq (Star (L 'B') Greedy) (Star (L 'A') Greedy))))

input10 =  "ABA"


-- pattern = <(x :: (0|...|9)+?)*, (y :: (0|...|9)+?)*, (z :: (0|...|9)+?)*>

digits_re = foldl' (\x y -> Choice x y Greedy) (L '0') (map L "12345789")

p11 = PPair (PStar (PVar 1 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PPair (PStar (PVar 2 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PPair (PStar (PVar 3 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PStar (PVar 4 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy)))

input11 =  "1234567890123456789-"


testBitCodeParseRegex1 = 
    let r4 = strip p4
        regex = buildRegex p4
    in testCase "parseRegex should be the same as parseBX" $ assertEqual [] (parseRegex regex "ABAAC") (Just [0,0,1])

testBitCodeP4 = 
    let r4 = strip p4
        trans = buildTrans r4
        regex = buildRegex p4
    in testCase "parseRegex should be the same as parseBX" $ assertEqual [] (parseRegex regex "ABAAC") (parseBX trans r4 "ABAAC")


testBitCodeP5 = 
    let r5 = strip p5
        trans = buildTrans r5
        regex = buildRegex p5
    in testCase "parseRegex should be the same as parseBX" $ assertEqual [] (parseRegex regex "ABAAC") (parseBX trans r5 "ABAAC")

testBitCodeP6 = 
    let r6 = strip p6
        trans = buildTrans r6
        regex = buildRegex p6
    in testCase "parseRegex should be the same as parseBX" $ assertEqual [] (parseRegex regex "ABAAC") (parseBX trans r6 "ABAAC")


testBitCodeP7 = 
    let r7 = strip p7
        trans = buildTrans r7
        regex = buildRegex p7
    in testCase "parseRegex should be the same as parseBX" $ assertEqual [] (parseRegex regex input7) (parseBX trans r7 input7)



testBitCodeP8 = 
    let r8 = strip p8
        trans = buildTrans r8
        regex = buildRegex p8
    in testCase "parseRegex should be the same as parseBX" $ assertEqual [] (parseRegex regex input8) (parseBX trans r8 input8)
