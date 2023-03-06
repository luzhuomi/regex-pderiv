module Text.Regex.PDeriv.BitCode.Bit where

import Text.Regex.PDeriv.BitCode.ParseTree
import Text.Regex.PDeriv.Common
import Text.Regex.PDeriv.RE

type Bits = [Int]


decode' :: RE -> Bits -> (U, Bits)
decode' Empty bs = (NilU, bs)
decode' (L a) bs = (Letter a, bs)
decode' (Choice r1 r2 f) (0:bs) = let (u, bs') = decode' r1 bs
                                in (LeftU u, bs')
decode' (Choice r1 r2 f) (1:bs) = let (u, bs') = decode' r2 bs
                                in (RightU u, bs')
decode' (Seq r1 r2) bs = let (u1, bs') = decode' r1 bs -- do we care about what matching policy here?
                             (u2, bs'') = decode' r2 bs'
                         in (Pair (u1,u2), bs'')
decode' (Star r f) (1:bs) = (List [], bs)
decode' (Star r f) (0:bs) = let (u, bs') = decode' r bs
                                (List us, bs'') = decode' (Star r f) bs'
                            in (List (u:us), bs'')
decode' r bs = error $ (show r) ++ " " ++ (show bs)

decode :: RE -> Bits -> U
decode r bs = case decode' r bs of
       (u, []) -> u
       (_, _ ) -> error "decode failed with non empty bits"




-- forward bits construction 
pderivBC :: RE -> Char -> [(RE, Bits)]
pderivBC Phi l = []
pderivBC Empty l = []
pderivBC (L l') l | l == l' = [(Empty, [])]
                  | otherwise = []
pderivBC (Choice r1 r2 f) l = [ (r1', 0:bs1) | (r1', bs1) <- pderivBC r1 l ] ++ [ (r2', 1:bs2) | (r2', bs2) <- pderivBC r2 l ]
pderivBC (Star r f) l = [(Seq r' (Star r f), 0:bs) | (r', bs) <- pderivBC r l] 
pderivBC (Seq r1 r2) l | posEpsilon r1 = [(Seq r1' r2, bs1) | (r1' ,bs1) <- pderivBC r1 l] ++
                                         [(r2', empCode ++bs2) | (r2', bs2) <- pderivBC r2 l, let empCode = mkEmptyBC r1]
                       | otherwise     = [(Seq r1' r2, bs1) | (r1', bs1) <- pderivBC r1 l]


-- mkEmptyBC:
-- precondition, r must be nullable
mkEmptyBC :: RE -> Bits
mkEmptyBC Empty = []
mkEmptyBC (Choice r1 r2 f) | posEpsilon r1 = 0:(mkEmptyBC r1)
                           | posEpsilon r2 = 1:(mkEmptyBC r2)
                           | otherwise = error "mkEmptyBC is called with not nullable re."
mkEmptyBC (Seq r1 r2) = mkEmptyBC r1  ++ mkEmptyBC r2
mkEmptyBC (Star r f) = [1]
