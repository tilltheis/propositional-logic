module Logic where

import qualified Data.Set as Set


data Formula t = T | F | Symbol String
               | Negation (Formula t)
               | Conjunction (Formula t) (Formula t)
               | Disjunction (Formula t) (Formula t)
               | Implication (Formula t) (Formula t)
               | Equivalence (Formula t) (Formula t)
               deriving (Show, Ord, Eq)



-- TYPES

data Fancy  = Fancy deriving Show
data Normal = Normal deriving Show
data NNF    = NNF deriving Show
data CNF    = CNF deriving Show
data DNF    = DNF deriving Show



-- TYPE CASTS

-- Unsave!
--
-- Formulae are casted regardless of their actual values.
-- E.g. "atom T `imply` atom T" can be casted to Formula Normal.

fancy :: Formula t -> Formula Fancy
fancy = cast

normal :: Formula t -> Formula Normal
normal = cast

nnf :: Formula t -> Formula NNF
nnf = cast

cnf :: Formula t -> Formula CNF
cnf = cast

dnf :: Formula t -> Formula DNF
dnf = cast


cast :: Formula t -> Formula u
cast = transform cast



-- TRANSFORMATIONS

transform :: (Formula t -> Formula u) -> Formula t -> Formula u
transform _ T = T
transform _ F = F
transform _ (Symbol s) = Symbol s
transform f (Negation x) = Negation $ f x
transform f (Conjunction x y) = Conjunction (f x) (f y)
transform f (Disjunction x y) = Disjunction (f x) (f y)
transform f (Implication x y) = Implication (f x) (f y)
transform f (Equivalence x y) = Equivalence (f x) (f y)


-- transform inner formulae first
deepTransform :: (Formula t -> Formula t) -> Formula t -> Formula t
deepTransform f x = f (transform (deepTransform f) x)


foldFormula :: (Formula t -> a -> a) -> a -> Formula t -> a
foldFormula f z T = f T z
foldFormula f z F = f F z
foldFormula f z val@(Symbol _) = f val z
foldFormula f z val@(Negation x) = f val z'
    where z' = foldFormula f z x
foldFormula f z val@(Conjunction x y) = foldConnective f z x y val
foldFormula f z val@(Disjunction x y) = foldConnective f z x y val
foldFormula f z val@(Implication x y) = foldConnective f z x y val
foldFormula f z val@(Equivalence x y) = foldConnective f z x y val

foldConnective f z x y val = f val z''
    where z'  = foldFormula f z y
          z'' = foldFormula f z' x


-- change connective
reconnect :: (Formula t -> Formula t -> a) -> Formula t -> a
reconnect f (Conjunction x y) = f x y
reconnect f (Disjunction x y) = f x y
reconnect f (Implication x y) = f x y
reconnect f (Equivalence x y) = f x y
reconnect _ _ = undefined



-- NORMAL FORMS

mkNormal :: Formula Fancy -> Formula Normal
mkNormal = normal . deepTransform mkNormalVal

mkNormalVal :: Formula t -> Formula t
mkNormalVal (Implication x y) = Negation x `Disjunction` y
mkNormalVal (Equivalence x y) =
    transform mkNormalVal $ Implication x y `Conjunction` Implication y x
mkNormalVal x = x



mkNNF :: Formula Normal -> Formula NNF
mkNNF = nnf . deepTransform doubleNegation . deepTransform deMorgan


deMorgan :: Formula t -> Formula t
deMorgan (Negation (Conjunction x y)) =
    transform deMorgan $ Negation x `Disjunction` Negation y
deMorgan (Negation (Disjunction x y)) =
    transform deMorgan $ Negation x `Conjunction` Negation y
deMorgan x = x


doubleNegation :: Formula t -> Formula t
doubleNegation (Negation (Negation x)) = x
doubleNegation x = x



mkCNF :: Formula NNF -> Formula CNF
mkCNF = cnf . deepTransform mkCNFVal

mkCNFVal :: Formula t -> Formula t
mkCNFVal (Disjunction x@(Conjunction _ _) y) =
    transform (mkCNFVal . Disjunction y) x
mkCNFVal (Disjunction y x@(Conjunction _ _)) = mkCNFVal $ Disjunction x y
mkCNFVal x = x



-- SIMPLIFICATION

-- (fromSet . toSet) takes care of idempotence
simplify :: Formula CNF -> Formula CNF
simplify = id





toList :: Formula CNF -> [[Formula CNF]]
toList x@(Conjunction _ _) = reconnectMap (++) toList x
toList x@(Disjunction _ _) = return . concat $ reconnectMap (++) toList x
toList x = [[x]]

fromList :: [[Formula CNF]] -> Formula CNF
fromList l = foldr1 Conjunction l'
    where l' = map (foldr1 Disjunction) l

reconnectMap :: (a -> a -> b) -> (Formula t -> a) -> Formula t -> b
reconnectMap fc fm x = fc (fm $ fst p) (fm $ snd p)
    where p = reconnect (,) x


toSet :: Formula CNF -> Set.Set (Set.Set (Formula CNF))
toSet x@(Conjunction _ _) = subSets x
toSet x@(Disjunction _ _) = Set.singleton . setConcat $ subSets x
toSet x = Set.singleton $ Set.singleton x

fromSet :: Set.Set (Set.Set (Formula CNF)) -> Formula CNF
fromSet set = setFoldr1 Conjunction set'
    where set' = Set.map (setFoldr1 Disjunction) set


subSets :: Formula CNF -> Set.Set (Set.Set (Formula CNF))
subSets = reconnectMap Set.union toSet


setConcat :: (Eq a, Ord a) => Set.Set (Set.Set a) -> Set.Set a
setConcat = setFoldr1 Set.union

setFoldr1 :: Ord a => (a -> a -> a) -> Set.Set a -> a
setFoldr1 f set = Set.fold f z (set Set.\\ Set.singleton z)
    where z = head $ Set.toList set

