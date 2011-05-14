{-|

This is the main module which implements the propositional 'Formula' type
itself and all the algorithms on it.

-}


module Logic where

import Prelude hiding (foldr1)
import Data.Foldable (foldr1)
import qualified Data.Set as Set


-- | Propositional formulae in any form are represented by the atomic formulae
-- 'T' (true), 'F' (false) and 'Symbol' and by nesting those within the basic
-- logical connectives.
--
-- Each formula has its form type @t@ attached, i.e. whether its a formula in the
-- negative normal form ('NNF') or any of the available formula types.
data Formula t = T                                   -- ^ Atom (true)
               | F                                   -- ^ Atom (false)
               | Symbol String                       -- ^ Atom
               | Negation (Formula t)                -- ^ Unary connective
               -- (sometimes treated as atom, sometimes as connective)
               | Conjunction (Formula t) (Formula t) -- ^ Connective
               | Disjunction (Formula t) (Formula t) -- ^ Connective
               | Implication (Formula t) (Formula t) -- ^ Connective
               | Equivalence (Formula t) (Formula t) -- ^ Connective
               deriving (Show, Ord, Eq)



-- * Types

-- | Default 'Formula' form type. Formulae of this type don't have any structure
-- constraints.
data Fancy  = Fancy deriving Show

-- | 'Formula'e in the 'Normal' form don't include 'Implication's or
-- 'Equivalence's as those connectives can be created from the other ones.
data Normal = Normal deriving Show

-- | The negative normal form is similiar to the 'Normal' 'Formula' type but
-- only allows 'Negation's on atoms.
data NNF    = NNF deriving Show

-- | The connective normal form extends the negative normal form in that
-- 'Disjunction's must not contain any 'Conjunction's.
data CNF    = CNF deriving Show

-- | The disjunctive normal form extends the negative normal form in that
-- 'Conjunction's must not contain any 'Disjunction's.
data DNF    = DNF deriving Show



-- * Type Casts

-- | /Unsave(!)/ cast a 'Formula' from any type to any other type.
-- Formulae are casted regardless of their actual values.
--
-- Bad example (remember the definition of 'Normal' which says that 'Normal'
-- 'Formulae' must not include 'Implication's):
--
-- >>> :t cast (T `Implication` T) :: Formula Normal
-- cast (T `Implication` T) :: Formula Normal :: Formula Normal
cast :: Formula t -> Formula u
cast = transform cast

-- | Cast any 'Formula' to a 'Fancy' 'Formula'. This is safe because 'Fancy'
-- allows all kinds of sub-'Formula'e.
fancy :: Formula t -> Formula Fancy
fancy = cast

-- | Cast any 'Formula' to a 'Normal' 'Formula'. The caller must ensure that
-- the given 'Formula' doesn't include sub-'Formula'e of incompatible types
-- (i.e. 'Fancy' with 'Implication's or 'Equivalence's).
normal :: Formula t -> Formula Normal
normal = cast

-- | Cast any 'Formula' to a 'NNF' 'Formula'. The caller must ensure that
-- the given 'Formula' doesn't include sub-'Formula'e of incompatible types.
nnf :: Formula t -> Formula NNF
nnf = cast

-- | Cast any 'Formula' to a 'CNF' 'Formula'. The caller must ensure that
-- the given 'Formula' doesn't include sub-'Formula'e of incompatible types.
cnf :: Formula t -> Formula CNF
cnf = cast

-- | Cast any 'Formula' to a 'DNF' 'Formula'. The caller must ensure that
-- the given 'Formula' doesn't include sub-'Formula'e of incompatible types.
dnf :: Formula t -> Formula DNF
dnf = cast



-- * Transformations

-- | Transform a 'Formula' by applying a function to the arguments of its
-- outer connective. Atoms will not be transformed.
--
-- Example:
--
-- > transform Negation (Conjunction T F) == Conjunction (Negation T) (Negation F)
transform :: (Formula t -> Formula u) -> Formula t -> Formula u
transform _ T = T
transform _ F = F
transform _ (Symbol s) = Symbol s
transform f (Negation x) = Negation $ f x
transform f (Conjunction x y) = Conjunction (f x) (f y)
transform f (Disjunction x y) = Disjunction (f x) (f y)
transform f (Implication x y) = Implication (f x) (f y)
transform f (Equivalence x y) = Equivalence (f x) (f y)


-- | Recursively 'transform' a 'Formula', beginning at the atoms. While the
-- normal 'transform' function only transforms the connective's arguments,
-- 'deepTransform' does also transform the connective itself.
deepTransform :: (Formula t -> Formula t) -> Formula t -> Formula t
deepTransform f x = f (transform (deepTransform f) x)


-- | Reduce a 'Formula' to a single value, beginning at the atoms. The order in
-- which the connective's arguments are traversed and combined is undefined.
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


-- | Change a 'Formula''s connective. The connective can be substituted with
-- another 'Formula' constructor or an arbitrary binary function. Note that
-- this function is only defined for binary connectives!
reconnect :: (Formula t -> Formula t -> a) -> Formula t -> a
reconnect f (Conjunction x y) = f x y
reconnect f (Disjunction x y) = f x y
reconnect f (Implication x y) = f x y
reconnect f (Equivalence x y) = f x y
reconnect _ _ = undefined



-- * Normal Forms

-- | Convert a 'Fancy' 'Formula' to 'Formula' in the 'Normal' form. All
-- 'Formula'e, except the 'Fancy' ones, are already in the 'Normal' form.
mkNormal :: Formula Fancy -> Formula Normal
mkNormal = normal . deepTransform mkNormalVal

-- | Translate 'Fancy' 'Formula' constructors to their 'Normal' equivalents.
-- This is a \'flat\' function and is meant to be used with 'deepTransform'.
mkNormalVal :: Formula t -> Formula t
mkNormalVal (Implication x y) = Negation x `Disjunction` y
mkNormalVal (Equivalence x y) =
    transform mkNormalVal $ Implication x y `Conjunction` Implication y x
mkNormalVal x = x



-- | Convert a 'Normal' 'Formula' to 'Formula' in the negative normal form. All
-- 'Formula'e, except the 'Fancy' and 'Normal' ones, are already in the 'NNF'
-- form.
mkNNF :: Formula Normal -> Formula NNF
mkNNF = nnf . deepTransform doubleNegation . deepTransform deMorgan


-- | Remove 'Negation's in front of connectives by moving the 'Negation' into
-- the connective (for more information please refer to
-- <http://en.wikipedia.org/wiki/De_Morgan%27s_laws>).
-- This is a \'flat\' function and is meant to be used with 'deepTransform'.
deMorgan :: Formula t -> Formula t
deMorgan (Negation (Conjunction x y)) =
    transform deMorgan $ Negation x `Disjunction` Negation y
deMorgan (Negation (Disjunction x y)) =
    transform deMorgan $ Negation x `Conjunction` Negation y
deMorgan x = x


-- | Remove double 'Negation's as they neutralize themselves.
-- This is a \'flat\' function and is meant to be used with 'deepTransform'.
doubleNegation :: Formula t -> Formula t
doubleNegation (Negation (Negation x)) = x
doubleNegation x = x



-- | Convert a 'Formula' in the negative normal form to a 'Formula' in the
-- conjunctive normal form.
mkCNF :: Formula NNF -> Formula CNF
mkCNF = cnf . deepTransform mkCNFVal

-- | Assure that no 'Disjunction' contains any 'Conjunction's by making use of
-- the distributive law (<http://en.wikipedia.org/wiki/Distributivity>).
-- This is a \'flat\' function and is meant to be used with 'deepTransform'.
mkCNFVal :: Formula t -> Formula t
mkCNFVal (Disjunction x@(Conjunction _ _) y) =
    transform (mkCNFVal . Disjunction y) x
mkCNFVal (Disjunction y x@(Conjunction _ _)) = mkCNFVal $ Disjunction x y
mkCNFVal x = x



-- * Simplification

-- | Reduce a 'Formula' in the conjunctive normal form to its bare minimum.

-- (fromSet . toSet) takes care of idempotence
simplify :: Formula CNF -> Formula CNF
simplify = id




-- | From a 'Formula' in the conjunctive normal form create a list (the
-- conjunctions) of lists (the disjunctions) with the atoms at the leafs.
toList :: Formula CNF -> [[Formula CNF]]
toList x@(Conjunction _ _) = reconnectMap (++) toList x
toList x@(Disjunction _ _) = return . concat $ reconnectMap (++) toList x
toList x = [[x]]

-- | Create a 'Formula' from a list, created by 'toList'.
fromList :: [[Formula CNF]] -> Formula CNF
fromList l = foldr1 Conjunction l'
    where l' = map (foldr1 Disjunction) l

-- | First 'transform' and then 'reconnect' a 'Formula' but don't require the
-- transformer to yield 'Formula' results.
reconnectMap :: (a -> a -> b) -> (Formula t -> a) -> Formula t -> b
reconnectMap fc fm x = fc (fm $ fst p) (fm $ snd p)
    where p = reconnect (,) x


-- | From a 'Formula' in the conjunctive normal form create a set (the
-- conjunctions) of sets (the disjunctions) with the atoms at the leafs.
toSet :: Formula CNF -> Set.Set (Set.Set (Formula CNF))
toSet x@(Conjunction _ _) = subSets x
toSet x@(Disjunction _ _) = Set.singleton . setConcat $ subSets x
toSet x = Set.singleton $ Set.singleton x

-- | Create a 'Formula' from a set, created by 'toSet'.
fromSet :: Set.Set (Set.Set (Formula CNF)) -> Formula CNF
fromSet set = foldr1 Conjunction set'
    where set' = Set.map (foldr1 Disjunction) set

-- | This function works like 'toSet' but pretends its argument is a
-- 'Conjunction'. It's a helper function of 'toSet'.
subSets :: Formula CNF -> Set.Set (Set.Set (Formula CNF))
subSets = reconnectMap Set.union toSet


-- | List's concat function for Sets.
setConcat :: (Eq a, Ord a) => Set.Set (Set.Set a) -> Set.Set a
setConcat = foldr1 Set.union
