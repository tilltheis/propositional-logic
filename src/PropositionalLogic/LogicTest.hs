{-# LANGUAGE FlexibleInstances #-}


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit

import Test.QuickCheck
import Test.HUnit


import PropositionalLogic
import PropositionalLogic.Logic

import Control.Monad (liftM, liftM2)
import Data.List (nub)


main = defaultMain tests

tests =
    [ testGroup "Normal Forms"
        [ testProperty "Normal Has Nothing Fancy\n"   prop_normalNoFancy
        , testProperty "NNF Has No Double Negation\n" prop_nnfNoDoubleNegation
        , testProperty "NNF Only Negates Atoms\n"     prop_nnfOnlyNegAtoms
        , testProperty "CNF Has No Conjunctions Within Disjunctions\n"
            prop_cnfNoConjInDisj
        , testProperty "DNF Has No Disjunctions Within Conjunctions\n"
            prop_dnfNoDisjInConj
        ]
    , testGroup "Misc"
        [ testProperty "pretty printing and reparsing doesn't change formulae\n"
            prop_printReadIsId
        , testProperty "simplified formula has no duplicate prime implicants\n"
            prop_simplifiedNoDupPIs
        ]
    , testGroup "Optimization Examples"
        [ testCase "tautology" test_tautology
        , testCase "simplify1" test_simplify1
        , testCase "simplify2" test_simplify2
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

dnfFormula :: Gen (Formula DNF)
dnfFormula = liftM mkDNF nnfFormula


instance Arbitrary (Formula Fancy) where
    arbitrary = fancyFormula

instance Arbitrary (Formula Normal) where
    arbitrary = normalFormula

instance Arbitrary (Formula NNF) where
    arbitrary = nnfFormula

instance Arbitrary (Formula CNF) where
    arbitrary = cnfFormula

instance Arbitrary (Formula DNF) where
    arbitrary = dnfFormula


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


prop_dnfNoDisjInConj :: Formula DNF -> Bool
prop_dnfNoDisjInConj = not . findDisjInConj

findDisjInConj :: Formula DNF -> Bool
findDisjInConj = foldFormula findDisjInConj' False

findDisjInConj' (Conjunction (Disjunction _ _) _) _ = True
findDisjInConj' (Conjunction _ (Disjunction _ _)) _ = True
findDisjInConj' _ x = x


prop_printReadIsId :: Formula Fancy -> Bool
prop_printReadIsId x = case formula $ prettyFormulaString x of
                            Right y -> truthTable x == truthTable y
                            Left _  -> False

prop_simplifiedNoDupPIs :: Formula Fancy -> Bool
prop_simplifiedNoDupPIs x = simplePIs == nub simplePIs
  where simplePIs = petrick . qm . qmMappings . trueMappings $ truthTable x


test_tautology = simplify x @?= T
  where x = Equivalence (Disjunction (Negation (Symbol "A")) (Symbol "B")) (Implication (Symbol "A") (Symbol "B")) -- !A v B <-> (A -> B)

test_simplify1 = simplify x @?= Disjunction (Symbol "B") (Symbol "C") -- B v C
  where x = Disjunction (Equivalence (Disjunction (Negation (Negation (Symbol "A"))) (Symbol "B")) (Implication (Symbol "A") (Symbol "B"))) (Symbol "C") -- (!!A v B <-> (A -> B)) v C

test_simplify2 = simplify x @?= Disjunction (Symbol "B") (Disjunction (Symbol "C") (Negation (Symbol "A"))) -- !A v B v C
  where x = Disjunction (Negation (Symbol "A")) (Disjunction (Symbol "B") (Symbol "C")) -- !A v B v C
