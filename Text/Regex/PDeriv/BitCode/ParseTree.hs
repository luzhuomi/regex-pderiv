module Text.Regex.PDeriv.BitCode.ParseTree where


import qualified Data.ByteString.Char8 as S
import Data.ByteString.Internal (c2w)
import Text.Regex.PDeriv.IntPattern

-- universal representation of regex patterns
-- [[r]] = u
data U where
  Nil :: U
  Empty :: U
  Letter :: Char -> U
  LeftU :: U -> U
  RightU :: U -> U
  Pair :: (U,U) -> U
  List :: [U] -> U
  deriving (Show, Eq)


type Word = S.ByteString

flatten :: U -> Word
flatten Nil = error "flatten: trying to flatten a phi into a word."
flatten Empty = S.empty
flatten (Letter c) = c2w c
flatten (LeftU u) = flatten u
flatten (RightU u) = flatten u
flatten (Pair (u,v)) = flatten u `S.append` flatten v
flatten (List us) = S.concat $ map flatten us


type Env = [(Int,Word)]


-- extract the match environment from the parse tree
parseTreeToEnv :: Pat -> U -> Env
parseTreeToEnv = undefined
  where ptToEnv :: Pat -> U
        ptToEnv (PVar i b p) u =
          let env = ptToEnv p u
          in (i,flatten u):env
        ptToEnv (PE r) _ = []
        ptToEnv (PPair p1 p2) (Pair (u1, u2)) = ptEnv p1 u1 ++ ptEnv p2 u2
        ptToEnv (PPlus p1 p2) (Pair (u1, u2)) = ptEnv p1 u1 ++ ptEnv p2 u2
        ptToEnv (PChoice p1 p2) (LeftU u) = ptEnv p1 u1
        ptToEnv (PChoice p1 p2) (RightU u) = ptEnv p2 u2
        

