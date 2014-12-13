module HarmLang.Probability
where

import Data.List
import Data.Function

-- funcs that know about implementation of Dist:
-- equally
-- weightedly
-- regroup
-- pmap
-- resolve
-- joint (needn't)
-- probv
-- pgroup
-- pcombine
-- maxlikelihood
-- sample

-------------
--- TYPES ---
-------------

-- distribution with finite support.
-- invariants: each value of type a is unique in the list
-- and sum (snd (unzip list)) == 1.0
data Dist a = Dist [(a, Double)] deriving (Show)

instance Eq a => Eq (Dist a) where
  (==) d1 d2 = (Prelude.==) (likelylist d1) (likelylist d2)

--------------------
--- CONSTRUCTORS ---
--------------------

certainly :: (Eq a) => a -> Dist a
certainly a = Dist [(a, 1.0)]

equally :: (Eq a) => [a] -> Dist a 
equally as = Dist $ regroup (zip as (repeat (1.0/(fromIntegral (length as)))))

weightedly :: (Eq a) => [(a, Double)] -> Dist a
weightedly as =
    let total = sum (snd (unzip as))
    in  Dist $ regroup (map (\(a, w) -> (a, w / total)) as)

-- combines elems with equal value by adding their probs
-- in order to limit the length of a dist to its support
regroup :: (Eq a) => [(a, Double)] -> [(a, Double)]
regroup [] = []
regroup (a:as) =
    let r [] y = [y]
        r ((v1, p1):xs) (v2, p2) =
            if v1 == v2
            then ((v1, p1 + p2):xs)
            else ((v1, p1):(r xs (v2, p2)))
    in r (regroup as) a

--------------------
--- TRANSFORMERS ---
--------------------

pmap :: (Eq a, Eq b) => (a -> b) -> Dist a -> Dist b
pmap f (Dist as) =
    let (vs, ps) = unzip as
    in Dist $ regroup (zip (map f vs) ps)

-- does not know about implementation of F
sumdist :: (Num a, Eq a) => Dist a -> Dist a -> Dist a
sumdist as bs = pmap (uncurry (+)) (joint as bs)

resolve :: (Eq a) => Dist (Dist a) -> Dist a
resolve (Dist []) = Dist []
--resolve (Dist (((Dist xs), p):ys)) = regroup ((map (\(v, p') -> (v, p * p')) xs) ++ (resolve ys))
resolve (Dist xs) =
    let collapse (Dist as, p) = map (\(v, p') -> (v, p * p')) as
    in  Dist $ regroup (foldr (++) [] (map collapse xs))

-- joint distribution where events of type b depend on events of type a
-- each (A, B) node holds the probability P(A ^ B) = P(A ^ B | A) * P(A)
jointdep :: (Eq a, Eq b) => Dist a -> (a -> Dist b) -> Dist (a, b)
jointdep as f =
    let f' a = pmap (\v -> (a, v)) (f a)
    in  resolve (pmap f' as)

-- TODO: rewrite with pmap so doesn't need to know about F
-- joint disribution for two independent events
joint :: (Eq a, Eq b) => Dist a -> Dist b -> Dist (a, b)
joint (Dist as) (Dist bs) = Dist [ ((a, b), pa * pb) | (a, pa) <- as, (b, pb) <- bs]

bind :: (Eq a, Eq b) => Dist a -> (a -> Dist b) -> Dist b
bind dist f = resolve $ pmap f dist

--bindx :: (Eq a, Eq b) => Dist a -> (a -> Dist b) -> Dist (a,b)

-----------------
--- OBSERVERS ---
-----------------

probv :: (Eq a) => Dist a -> a -> Double
probv (Dist []) _ = 0
probv (Dist xs) val =
    let vp ((v, p):xs) v' =
            if v == v'
            then p
            else vp xs v'
        vp ([]) v' = 0
    in  vp xs val

-- prob of seeing a value that matches the predicate
prob :: Eq a => (a -> Bool) -> Dist a -> Double
prob f as = probv (pmap f as) True
--TODO this is a slow way to implement this function.

support :: Dist a -> [a]
support (Dist as) = fst (unzip as)

-- gives a list of values paired with the probs, sorted
likelylist :: Dist a -> [(a, Double)]
likelylist (Dist xs) = 
    let sort ys = reverse (sortBy (compare `on` snd) ys)
    in sort xs

expected :: (Eq a) => (a -> Double) -> Dist a -> Double
expected f as = 
    let pfold g v (Dist xs) = foldr g v xs
    in  pfold (\(v, p) x -> v * p + x) 0 (pmap f as)

-- group elements into an inner distribution by an equivalence relation
pgroup :: (Eq a) => (a -> a -> Bool) -> Dist a -> Dist (Dist a)
pgroup eq (Dist xs) =
    let g _ [] = []
        g rel ys =
            let (fg, rest) = splitOffFirstGroup (\(x, _) (y, _) -> rel x y) ys
            in  (((weightedly fg), sum (snd (unzip fg))):(g rel rest))
    in Dist (g eq xs)

-- combines related elements by adding their probabilities
-- if v `eq` v', probabilies are combined and stored under v
pcombine :: (Eq a) => (a -> a -> Bool) -> Dist a -> Dist a
pcombine eq (Dist as) =
    let r rel [] y = [y]
        r rel ((v, p):xs) (v', p') =
            if v `rel` v'
            then ((v, p + p'):xs)
            else ((v, p):(r rel xs (v', p')))
        c _ [] = []
        c rel (x:xs) = r rel (c rel xs) x
    in  Dist (c eq as)

pfilter :: (Eq a) => (a -> Bool) -> Dist a -> Dist a
pfilter f (Dist as) = weightedly $ filter (\(a, _) -> f a) as

maxlikelihood :: (Eq a) => Dist a -> a
maxlikelihood (Dist distList) = let (item, _) = maximumBy (\ (_, prob1) (_, prob2) -> (compare) prob1 prob2) distList in item

--TODO

--sample ::

--TODO Use randomness monad?

----------------------
--- MISC FUNCTIONS ---
----------------------

-- combines probabilities for e.g. (d1, d2) and (d2, d1), as order doesn't matter
combalike :: (Eq a) => Dist (a, a) -> Dist (a, a)
combalike f = pcombine (\v@(w, x) v'@(y, z) -> v == v' || v == (z, y)) f

-- from https://stackoverflow.com/questions/8262179/group-list-by-equivalence-relation
splitOffFirstGroup :: (a -> a -> Bool) -> [a] -> ([a],[a])
splitOffFirstGroup equal (x:xs) = partition (equal x) (x:xs)
splitOffFirstGroup _     []       = ([],[])
-- "quotient set" - groups the given list by an equivalence relation
qset _ [] = []
qset eq xs =
    let (fg,rst) = splitOffFirstGroup eq xs
    in fg : qset eq rst

-- bad, too many grouping functions D:
pqset :: (Eq a) => (a -> a -> Bool) -> Dist a -> Dist a
pqset eq dist = pmap (\(Dist as) -> fst (head as)) (pgroup eq dist)

-- maybe remove
joint3 :: (Eq a, Eq b, Eq c) => Dist a -> Dist b -> Dist c -> Dist (a, b, c)
joint3 (Dist as) (Dist bs) (Dist cs) = Dist [ ((a, b, c), pa * pb * pc) | (a, pa) <- as, (b, pb) <- bs, (c, pc) <- cs]


{-
bindx :: Dist a -> (a -> Dist b) -> Dist b

joint :: (Eq a, Eq b) => Dist a -> Dist b -> Dist (a, b)
joint (Dist as) (Dist bs) = Dist [ ((a, b), pa * pb) | (a, pa) <- as, (b, pb) <- bs]
-}

choose :: (Eq a) => Double -> Dist a -> Dist a -> Dist a
choose val (Dist l1) (Dist l2) =
  let
    dmult mVal = map (\(a, b) -> (a, b * mVal))
  in
    Dist (regroup ((dmult val l1) ++ (dmult (1 - val) l2)))


