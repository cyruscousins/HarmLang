module HarmLang.ConditionalProbability where

import HarmLang.Probability

import qualified Data.Map 

type ConditionalDist ev dat = ev -> Dist dat

type Transformer ev ev' dat dat' = ev -> (ev', dat -> dat', dat' -> dat)


buildConditionalDist :: (Ord ev, Eq dat) => [(ev, dat)] -> ConditionalDist ev dat
buildConditionalDist samples =
  let listValLists = map (\ (key, val) -> (key, [val])) samples
      mapAll = Data.Map.fromListWith (++) listValLists
      --mapAllSorted = map List.sort mapAll
      --mapAllDist = if priorWeight == 0
      --             then Data.Map.map equally mapAll
      --             else Data.Map.mapWithKey (\ before afters -> choose (ddiv priorWeight (dplus priorWeight (fromIntegral $ length afters))) (prior before) (equally afters)) mapAll
      mapAllDist = Data.Map.map equally mapAll
  in \e -> (Data.Map.!) mapAllDist e

--buildConditionalDist dat = \e -> equally (map snd dat) -- Unconditional

--applyTransformer :: Transformer ev ev' dat dat' -> ConditionalDist ev dat -> ConditionalDist ev dat
--applyTransformer transformer dist = 


buildTransformedConditionalDist :: (Ord ev, Ord ev', Eq dat, Eq dat') => Transformer ev ev' dat dat' -> [(ev, dat)] -> ConditionalDist ev dat
buildTransformedConditionalDist t dat = 
  let toInner (ev, dat) = let (innerEv, dTrans, _) = t ev in (innerEv, dTrans dat)
      innerDist = buildConditionalDist $ map toInner dat
      outerDist e = let (innerE, _, dInvTrans) = t e in pmap dInvTrans (innerDist innerE)
  in outerDist


cdChoose :: (Eq dat) => Double -> ConditionalDist ev dat -> ConditionalDist ev dat -> ConditionalDist ev dat
cdChoose d d1 d2 = \e -> choose d (d1 e) (d2 e)






intTrans :: Transformer [Int] [Int] Int Int
intTrans ev@(h:_) = (map ((-) h) ev, ((-) h), ((+) h))


intData :: [([Int], Int)]
intData = [([0,1],1),([1,2],0),([2,3],1),([0,0],0)]
