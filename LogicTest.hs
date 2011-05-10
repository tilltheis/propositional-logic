{-# LANGUAGE FlexibleInstances #-}


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit


import Logic

import Control.Monad (liftM, liftM2)


main = defaultMain tests

tests =
    [ testGroup "Normal Forms"
        [ testProperty "Normal Has Nothing Fancy\n"   prop_normalNoFancy
        , testProperty "NNF Has No Double Negation\n" prop_nnfNoDoubleNegation
        , testProperty "NNF Only Negates Atoms\n"     prop_nnfOnlyNegAtoms
        , testProperty "CNF Has No Conjunctions Within Disjunctions\n"
            prop_cnfNoConjInDisj
        ]
    ]



fancyFormula :: Gen (Formula Fancy)
fancyFormula = sized fancyFormula'
fancyFormula' 0 =
    oneof [ return T
          , return F
          , return (Symbol "X")
          ]
fancyFormula' n | n > 0 =
    oneof [ liftM Negation subFormula
          , liftM2 Conjunction subFormula subFormula
          , liftM2 Disjunction subFormula subFormula
          , liftM2 Implication subFormula subFormula
          , liftM2 Equivalence subFormula subFormula
          ]
    where subFormula = fancyFormula' (n `div` 5)


normalFormula :: Gen (Formula Normal)
normalFormula = liftM mkNormal fancyFormula

nnfFormula :: Gen (Formula NNF)
nnfFormula = liftM mkNNF normalFormula

cnfFormula :: Gen (Formula CNF)
cnfFormula = liftM mkCNF nnfFormula


instance Arbitrary (Formula Fancy) where
    arbitrary = fancyFormula

instance Arbitrary (Formula Normal) where
    arbitrary = normalFormula

instance Arbitrary (Formula NNF) where
    arbitrary = nnfFormula

instance Arbitrary (Formula CNF) where
    arbitrary = cnfFormula


prop_normalNoFancy :: Formula Normal -> Bool
prop_normalNoFancy = not . findFancyConstr

findFancyConstr :: Formula Normal -> Bool
findFancyConstr = foldFormula findFancyConstr' False
findFancyConstr' (Implication _ _) _ = True
findFancyConstr' (Equivalence _ _) _ = True
findFancyConstr' _ x = x



prop_nnfNoDoubleNegation :: Formula NNF -> Bool
prop_nnfNoDoubleNegation = not . findDoubleNegation

findDoubleNegation :: Formula NNF -> Bool
findDoubleNegation = foldFormula findDoubleNegation' False
findDoubleNegation' (Negation (Negation _)) _ = True
findDoubleNegation' _ x = x


prop_nnfOnlyNegAtoms :: Formula NNF -> Bool
prop_nnfOnlyNegAtoms = not . findNegNonAtom

findNegNonAtom :: Formula NNF -> Bool
findNegNonAtom = foldFormula findNegNonAtom' False

findNegNonAtom' (Negation T) x = x
findNegNonAtom' (Negation F) x = x
findNegNonAtom' (Negation (Symbol _)) x = x
findNegNonAtom' (Negation _) _ = True
findNegNonAtom' _ x = x


prop_cnfNoConjInDisj :: Formula CNF -> Bool
prop_cnfNoConjInDisj = not . findConjInDisj

findConjInDisj :: Formula CNF -> Bool
findConjInDisj = foldFormula findConjInDisj' False

findConjInDisj' (Disjunction (Conjunction _ _) _) _ = True
findConjInDisj' (Disjunction _ (Conjunction _ _)) _ = True
findConjInDisj' _ x = x


