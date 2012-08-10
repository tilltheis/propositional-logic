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
  , formula
  , prettyFormulaString
  ) where

import PropositionalLogic.Logic
import PropositionalLogic.Parser

prettyFormulaString :: Formula a -> String
prettyFormulaString T = "1"
prettyFormulaString F = "0"
prettyFormulaString (Symbol s) = s
prettyFormulaString (Negation f) = "!" ++ prettyFormulaString f
prettyFormulaString (Conjunction f g) = "(" ++ prettyFormulaString f ++ ") ^ (" ++ prettyFormulaString g ++ ")"
prettyFormulaString (Disjunction f g) = "(" ++ prettyFormulaString f ++ ") v (" ++ prettyFormulaString g ++ ")"
prettyFormulaString (Implication f g) = "(" ++ prettyFormulaString f ++ ") -> (" ++ prettyFormulaString g ++ ")"
prettyFormulaString (Equivalence f g) = "(" ++ prettyFormulaString f ++ ") <-> (" ++ prettyFormulaString g ++ ")"