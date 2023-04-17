{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 

module Text.Regex.PDeriv.BitCode.Transduce where


import Text.Regex.PDeriv.BitCode.Bit
import Text.Regex.PDeriv.BitCode.ParseTree
import Text.Regex.PDeriv.Common (Range(..), Letter, PosEpsilon(..), Simplifiable(..), my_hash, my_lookup, GFlag(..), nub2, preBinder, mainBinder, subBinder)
import Text.Regex.PDeriv.IntPattern hiding (nub2)
import Text.Regex.PDeriv.RE
import Text.Regex.PDeriv.Parse


import Data.List 
import qualified Data.Map as DM
import qualified Data.IntMap.Strict as IM
import qualified Data.ByteString.Char8 as S
import Debug.Trace

-- mapping (src, l) to [(dst, bits_to_be_emitted)]
type Trans = DM.Map (RE, Char) [(RE, Bits)]

buildTrans :: RE -> Trans
buildTrans r =
           let sig = sigmaRE r
               init_states = [r]
               (all_states, trans) = buildFix [] init_states DM.empty sig -- buildFix init_states DM.empty sig
           in trans

-- pre cond 1  allStatesSoFar \insect newStates = \emptyset
--          2  dom(dom(currTrans)) \in allStatesFoFar
buildFix :: [RE] -> [RE] -> Trans -> [Char] -> ([RE], Trans)
buildFix allStatesSoFar [] currTrans sig = (allStatesSoFar, currTrans)
buildFix allStatesSoFar newStates currTrans sig =
  let newDeltaBits = concat $ do
        r <- newStates
        l <- sig
        let tfs = pderivBC r l
            tfdelta = map (\(t,bc) -> (r,l,t,bc)) tfs
        return (filter (\(r,l,t,bc) -> not ((r,l) `DM.member` currTrans)) tfdelta)
      allStatesNext = allStatesSoFar ++ newStates
      newStatesNext = nub $ sort (map (\(r,l,t,bc) -> t) newDeltaBits)
      nextTrans     = foldl (\ trans (r,l,t,bc) ->
                                if (r,l) `DM.member` trans
                                then DM.update (\rxs -> Just $ rxs ++ [(t,bc)]) (r,l) trans
                                else DM.insert (r,l) [(t,bc)] trans ) currTrans newDeltaBits
  in buildFix allStatesNext newStatesNext nextTrans sig


{-
buildFix :: [RE] -> Trans -> [Char] -> ([RE], Trans)
buildFix allStatesSoFar currTrans sig = 
         let newDeltaBits = concat $ do
             r <- allStatesSoFar
             l <- sig
             let tfs = pderivBC r l
                 tfdelta = map (\(t,bc) -> (r,l,t,bc)) tfs
             return (filter (\(r,l,t,bc) -> not ((r,l) `DM.member` currTrans)) tfdelta)
         in if null newDeltaBits
            then (allStatesSoFar, currTrans)
            else let nextStatesSoFar = nub $ sort $ allStatesSoFar ++ (map (\(r,l,t,bc) -> t) newDeltaBits)
                     nextTrans = foldl (\ trans (r,l,t,bc) ->
                                             if (r,l) `DM.member` trans
                                             then DM.update (\rxs -> Just $ rxs ++ [(t,bc)]) (r,l) trans
                                             else DM.insert (r,l) [(t,bc)] trans ) currTrans newDeltaBits
                 in buildFix nextStatesSoFar nextTrans sig

-}

parseBX :: Trans -> RE -> [Char] -> Maybe Bits
parseBX trans r cs = case go [(r,[])] cs of
      [] -> Nothing
      (bc:_) -> Just bc
      where go :: [(RE, Bits)] -> [Char] -> [Bits]
            go rbc [] = concatMap (\(r, bc) -> if posEpsilon r
                                     then [bc ++ (mkEmptyBC r)]
                                     else []) rbc
            go rbc (x:xs) = let tbc :: [(RE,Bits)]
                                tbc = concatMap g rbc
                                g :: (RE, Bits) -> [(RE,Bits)]
                                g (r, bc) = case DM.lookup (r,x) trans of
                                            { Nothing -> []
                                            ; Just tfs -> map (\(t,bc') -> (t, bc++bc')) tfs
                                            }
                            in go tbc xs


type ITrans = IM.IntMap  [(Int, Bits)]
type Finals = IM.IntMap  Bits
type Init = Int

newtype Regex = Regex (ITrans, Init, Finals, Pat)


numTrans :: Regex -> Int
numTrans (Regex (ts,_,_,_)) = IM.size ts

srcPat :: Regex -> Pat
srcPat (Regex (_,_,_,p) ) = p


buildRegex :: Pat -> Regex
buildRegex p =
            let r = strip p
                sig = sigmaRE r
                init_states = [r]
                (all_states, trans) = buildFix [] init_states DM.empty sig -- buildFix init_states DM.empty sig
                all_states_and_ids = zip all_states [0..]
                re2id = DM.fromList all_states_and_ids
                id2re = DM.fromList (map (\ (x,y) -> (y,x)) all_states_and_ids)
                itrans :: ITrans
                itrans = foldl (\tbl ((r,c),tbc)  ->
                                let ri = re2id DM.! r
                                    itbc = map (\(t,bc) -> (re2id DM.! t, bc)  ) tbc
                                    key = my_hash ri c
                                in case IM.lookup key tbl of
                                   Just ibits -> IM.update (\_ -> Just $ itbc ++ ibits) key tbl
                                   Nothing    -> IM.insert key itbc tbl) IM.empty (DM.toList trans)
                finals = IM.fromList (map (\r -> (re2id DM.! r, mkEmptyBC r)) (filter posEpsilon all_states))
             in Regex (itrans, (re2id DM.! r), finals, p)
                                  


parseRegex :: Regex -> [Char] -> Maybe Bits
parseRegex (Regex (itrans,init_istate,finals, _)) cs = case go [(init_istate,[])] cs of
      [] -> Nothing
      (bc:_) -> Just bc
      where go :: [(Int, Bits)] -> [Char] -> [Bits]
            go rbc [] = concatMap (\(r, bc) -> case IM.lookup r finals of
                                     { Just emptybits -> [bc ++ emptybits]
                                     ; Nothing -> [] }) rbc
            go rbc (x:xs) = let tbc :: [(Int,Bits)]
                                tbc = concatMap g rbc
                                g :: (Int, Bits) -> [(Int,Bits)]
                                g (r, bc) = case IM.lookup (my_hash r x) itrans of
                                            { Nothing -> []
                                            ; Just tfs -> map (\(t,bc') -> (t, bc++bc')) tfs
                                            }
                            in go tbc xs


-- ByteString variant
parseRegexBS :: Regex -> S.ByteString -> Maybe Bits
parseRegexBS (Regex (itrans,init_istate,finals, _)) cs = case go [(init_istate,[])] cs of
      [] -> Nothing
      (bc:_) -> Just bc
      where go :: [(Int, Bits)] -> S.ByteString -> [Bits]
            go rbc bs = case S.uncons bs of
            { Nothing -> concatMap (\(r, bc) -> case IM.lookup r finals of
                                     { Just emptybits -> [bc ++ emptybits]
                                     ; Nothing -> [] }) rbc
            ; Just (x,xs)-> let tbc :: [(Int,Bits)]
                                tbc = nub2 $ concatMap g rbc
                                g :: (Int, Bits) -> [(Int,Bits)]
                                g (r, bc) = case IM.lookup (my_hash r x) itrans of
                                            { Nothing -> []
                                            ; Just tfs -> map (\(t,bc') -> (t, bc++bc')) tfs
                                            }
                            in go tbc xs
             }                            


compile :: S.ByteString -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile bs =
    case parsePat (S.unpack bs) of
    Left err -> Left ("parseRegex for Text.Regex.PDeriv.ByteString failed:"++show err)
    Right pat -> Right (build pat)
    where 
      build p  = buildRegex $ simplify p


regexec :: Regex      -- ^ Compiled regular expression
       -> S.ByteString -- ^ ByteString to match against
       -> Either String (Maybe (S.ByteString, S.ByteString, S.ByteString, [S.ByteString]))
regexec r bs =
 case parseRegexBS r bs of
   Nothing -> Right (Nothing)
   Just bc ->
     let env = decodePat (srcPat r) bc bs 
         pre = case lookup preBinder env of { Just w -> w ; Nothing -> S.empty }
         post = case lookup subBinder env of { Just w -> w ; Nothing -> S.empty }
         main = case lookup mainBinder env of { Just w -> w ; Nothing -> S.empty }
         matched = map snd (filter (\(v,w) -> v > 0) env)
     in Right (Just (pre,main,post,matched))

decodePat :: Pat -> Bits -> S.ByteString -> Env
decodePat p bc s =
          let r = strip p
              u = decode r bc s
          in parseTreeToEnv p u
