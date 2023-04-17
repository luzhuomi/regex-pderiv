module Text.Regex.PDeriv.BitCode.ParseTree where


import qualified Data.ByteString.Char8 as S
import Text.Regex.PDeriv.IntPattern

-- universal representation of regex patterns
-- [[r]] = u
data U where
  NilU :: U
  AnyU :: Char -> U
  Letter :: Char -> U
  LeftU :: U -> U
  RightU :: U -> U
  Pair :: (U,U) -> U
  List :: [U] -> U
  deriving (Show, Eq)


type W = S.ByteString

flatten :: U -> W
flatten NilU = S.empty
flatten (Letter c) = S.singleton c
flatten (AnyU c)= S.singleton c
flatten (LeftU u) = flatten u
flatten (RightU u) = flatten u
flatten (Pair (u,v)) = flatten u `S.append` flatten v
flatten (List us) = S.concat $ map flatten us


type Env = [(Int,W)]


-- extract the match environment from the parse tree
parseTreeToEnv :: Pat -> U -> Env
parseTreeToEnv p u = ptEnv p u
  where ptEnv :: Pat -> U -> Env
        ptEnv (PVar i b p) u =
          let env = ptEnv p u
          in (i,flatten u):env
        ptEnv (PE r) _ = []
        ptEnv (PPair p1 p2) (Pair (u1, u2)) = ptEnv p1 u1 ++ ptEnv p2 u2
        ptEnv (PPlus p1 p2) (Pair (u1, u2)) = ptEnv p1 u1 ++ ptEnv p2 u2
        ptEnv (PChoice p1 p2 flag) (LeftU u) = ptEnv p1 u
        ptEnv (PChoice p1 p2 flag) (RightU u) = ptEnv p2 u
        ptEnv (PStar p flag) (List us) = concatMap (ptEnv p) us
        ptEnv (PEmpty p) _ = []



