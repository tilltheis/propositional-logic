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
  ) where

import PropositionalLogic.Logic
import PropositionalLogic.Parser