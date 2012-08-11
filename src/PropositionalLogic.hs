{- |
Module      :  $Header$
Description :  Propositional Formula operations with parsing, normal forms and optimization
Copyright   :  (c) Till Theis
License     :  MIT

Maintainer  :  Till Theis <theis.till@gmail.com>
Stability   :  experimental
Portability :  portable

Propositional Formula operations with parsing, normal forms and optimization.

-}

module PropositionalLogic
  ( Formula (..)
  , Fancy
  , Normal
  , NNF
  , CNF
  , DNF
  , mkNormal
  , mkNNF
  , mkCNF
  , mkDNF
  , simplifyCNF
  , simplifyDNF
  , simplify
  , formula
  , prettyFormulaString
  ) where

import PropositionalLogic.Logic
import PropositionalLogic.Parser

prettyFormulaString :: Formula a -> String
prettyFormulaString T = "1"
prettyFormulaString F = "0"
prettyFormulaString (Symbol s) = s
prettyFormulaString (Negation f) = "!" ++ parenthesize (const False) f
prettyFormulaString (Conjunction f g) = parenthesize isConjunction f ++ " ^ " ++ parenthesize isConjunction g
prettyFormulaString (Disjunction f g) = parenthesize isDisjunction f ++ " v " ++ parenthesize isDisjunction g
prettyFormulaString (Implication f g) = "(" ++ prettyFormulaString f ++ ") -> (" ++ prettyFormulaString g ++ ")"
prettyFormulaString (Equivalence f g) = "(" ++ prettyFormulaString f ++ ") <-> (" ++ prettyFormulaString g ++ ")"

parenthesize p x | isAtom x || p x = prettyFormulaString x
                 | otherwise       = "(" ++ prettyFormulaString x ++ ")"

isAtom T = True
isAtom F = True
isAtom (Symbol _) = True
isAtom (Negation _) = True
isAtom _ = False

isConjunction (Conjunction _ _) = True
isConjunction _ = False

isDisjunction (Disjunction _ _) = True
isDisjunction _ = False