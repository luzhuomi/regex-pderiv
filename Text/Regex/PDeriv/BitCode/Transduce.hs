{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 

module Text.Regex.PDeriv.BitCode.Transduce where


import Text.Regex.PDeriv.BitCode.Bit
import qualified Data.Map as DM
import qualified Data.IntMap as IM
import Text.Regex.PDeriv.Common (Range(..), Letter, PosEpsilon(..), Simplifiable(..), my_hash, my_lookup, GFlag(..), nub2, preBinder, mainBinder, subBinder)
import Text.Regex.PDeriv.IntPattern
import Text.Regex.PDeriv.RE
import Data.List 


-- mapping (src, l) to [(dst, bits_to_be_emitted)]
type Trans = DM.Map (RE, Char) [(RE, Bits)]

buildTrans :: RE -> Trans
buildTrans r =
           let sig = sigmaRE r
               init_states = [r]
               (all_states, trans) = buildFix init_states DM.empty sig
           in trans


buildFix :: [RE] -> Trans -> [Char] -> ([RE], Trans)
buildFix allStatesSoFar currTrans sig = 
         let newDeltaBits = concat $ do
             r <- allStatesSoFar
             l <- sig
             let tfs = pderivBC r l
                 tfdelta = map (\(t,bs) -> (r,l,t,bs)) tfs
             return (filter (\(r,l,t,bs) -> not ((r,l) `DM.member` currTrans)) tfdelta)
         in if null newDeltaBits
            then (allStatesSoFar, currTrans)
            else let nextStatesSoFar = nub $ sort $ allStatesSoFar ++ (map (\(r,l,t,bs) -> t) newDeltaBits)
                     nextTrans = foldl (\ trans (r,l,t,bs) ->
                                             if (r,l) `DM.member` trans
                                             then DM.update (\rxs -> Just $ rxs ++ [(t,bs)]) (r,l) trans
                                             else DM.insert (r,l) [(t,bs)] trans ) currTrans newDeltaBits
                 in buildFix nextStatesSoFar nextTrans sig


parseBX :: Trans -> RE -> [Char] -> Maybe Bits
parseBX trans r cs = case go [(r,[])] cs of
      [] -> Nothing
      (bs:_) -> Just bs
      where go :: [(RE, Bits)] -> [Char] -> [Bits]
            go rbs [] = concatMap (\(r, bs) -> if posEpsilon r
                                     then [bs ++ (mkEmptyBC r)]
                                     else []) rbs
            go rbs (x:xs) = let tbs :: [(RE,Bits)]
                                tbs = concatMap g rbs
                                g :: (RE, Bits) -> [(RE,Bits)]
                                g (r, bs) = case DM.lookup (r,x) trans of
                                            { Nothing -> []
                                            ; Just tfs -> map (\(t,bs') -> (t, bs++bs')) tfs
                                            }
                            in go tbs xs


type ITrans = IM.IntMap  [(Int, Bits)]
type Finals = IM.IntMap  Bits

newtype Regex = Regex (ITrans, Finals)

init_istate = 0

buildRegex :: Pat -> Regex
buildRegex p =
            let r = strip p
                sig = sigmaRE r
                init_states = [r]
                (all_states, trans) = buildFix init_states DM.empty sig
                all_states_and_ids = zip all_states [init_istate..]
                re2id = DM.fromList all_states_and_ids
                id2re = DM.fromList (map (\ (x,y) -> (y,x)) all_states_and_ids)
                itrans :: ITrans
                itrans = foldl (\tbl ((r,c),tbs)  ->
                                let ri = re2id DM.! r
                                    itbs = map (\(t,bs) -> (re2id DM.! t, bs)  ) tbs
                                    key = my_hash ri c
                                in case IM.lookup key tbl of
                                   Just ibits -> IM.update (\_ -> Just $ itbs ++ ibits) key tbl
                                   Nothing    -> IM.insert key itbs tbl) IM.empty (DM.toList trans)
                finals = IM.fromList (map (\r -> (re2id DM.! r, mkEmptyBC r)) (filter posEpsilon all_states))
             in Regex (itrans, finals)
                                  


parseRegex :: Regex -> [Char] -> Maybe Bits
parseRegex (Regex (itrans,finals)) cs = case go [(init_istate,[])] cs of
      [] -> Nothing
      (bs:_) -> Just bs
      where go :: [(Int, Bits)] -> [Char] -> [Bits]
            go rbs [] = concatMap (\(r, bs) -> case IM.lookup r finals of
                                     { Just emptybits -> [bs ++ emptybits]
                                     ; Nothing -> [] }) rbs
            go rbs (x:xs) = let tbs :: [(Int,Bits)]
                                tbs = concatMap g rbs
                                g :: (Int, Bits) -> [(Int,Bits)]
                                g (r, bs) = case IM.lookup (my_hash r x) itrans of
                                            { Nothing -> []
                                            ; Just tfs -> map (\(t,bs') -> (t, bs++bs')) tfs
                                            }
                            in go tbs xs


p4 = PPair (PPair p_x p_y) p_z
   where p_x = PVar 1 [] (PE (Choice (L 'A') (Seq (L 'A') (L 'B')) Greedy))      
         p_y = PVar 2 [] (PE (Choice (Seq (L 'B') (Seq (L 'A') (L 'A'))) (L 'A') Greedy))
         p_z = PVar 3 [] (PE (Choice (Seq (L 'A') (L 'C')) (L 'C') Greedy))

