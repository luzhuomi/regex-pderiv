module Text.Regex.PDeriv.BitCode.Bit where

import Text.Regex.PDeriv.BitCode.ParseTree
import Text.Regex.PDeriv.Common
import Text.Regex.PDeriv.RE


import qualified Data.ByteString.Char8 as S

type Bits = [Int]


-- we need to thread the ByteString to supply the character for Any regex
decode' :: RE -> Bits -> S.ByteString -> (U, Bits, S.ByteString)
decode' Empty bs s = (NilU, bs, s)
decode' (L a) bs s = (Letter a, bs, S.tail s)
decode' Any bs s = (AnyU (S.head s), bs, S.tail s)
decode' (Choice r1 r2 f) (0:bs) s = let (u, bs', s') = decode' r1 bs s
                                    in (LeftU u, bs', s')
decode' (Choice r1 r2 f) (1:bs) s = let (u, bs', s') = decode' r2 bs s
                                    in (RightU u, bs', s')
decode' (Seq r1 r2) bs s = let (u1, bs', s') = decode' r1 bs s -- do we care about what matching policy here?
                               (u2, bs'', s'') = decode' r2 bs' s'
                           in (Pair (u1,u2), bs'', s'')
decode' (Star r f) (1:bs) s = (List [], bs, s)
decode' (Star r f) (0:bs) s = let (u, bs', s') = decode' r bs s
                                  (List us, bs'', s'') = decode' (Star r f) bs' s'
                              in (List (u:us), bs'', s'')
decode' r bs s = error $ (show r) ++ " " ++ (show bs) ++ " " ++ (show s)

decode :: RE -> Bits -> S.ByteString  -> U
decode r bs s = case decode' r bs s of
       (u, [], s') | S.null s' -> u
       (_, _ , s') -> error "decode failed with non empty bits or non empty bytestring"




-- forward bits construction 
pderivBC :: RE -> Char -> [(RE, Bits)]
pderivBC Phi l = []
pderivBC Empty l = []
pderivBC (L l') l | l == l' = [(Empty, [])]
                  | otherwise = []
pderivBC Any l = [(Empty, [])]
pderivBC (Choice r1 r2 f) l = [ (r1', 0:bs1) | (r1', bs1) <- pderivBC r1 l ] ++ [ (r2', 1:bs2) | (r2', bs2) <- pderivBC r2 l ]
pderivBC (Star r f) l = [(Seq r' (Star r f), 0:bs) | (r', bs) <- pderivBC r l] 
pderivBC (Seq r1 r2) l | posEpsilon r1 = [(Seq r1' r2, bs1) | (r1' ,bs1) <- pderivBC r1 l] ++
                                         [(r2', empCode ++bs2) | (r2', bs2) <- pderivBC r2 l, let empCode = mkEmptyBC r1]
                       | otherwise     = [(Seq r1' r2, bs1) | (r1', bs1) <- pderivBC r1 l]
pderivBC r _ = error $  "pderivBC can't handle " ++  show r                        


-- mkEmptyBC:
-- precondition, r must be nullable
mkEmptyBC :: RE -> Bits
mkEmptyBC Empty = []
mkEmptyBC (Choice r1 r2 f) | posEpsilon r1 = 0:(mkEmptyBC r1)
                           | posEpsilon r2 = 1:(mkEmptyBC r2)
                           | otherwise = error "mkEmptyBC is called with not nullable re."
mkEmptyBC (Seq r1 r2) = mkEmptyBC r1  ++ mkEmptyBC r2
mkEmptyBC (Star r f) = [1]


