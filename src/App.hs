import Control.Monad

import Haste
import Haste.DOM
import PropositionalLogic

main = do
  withElem "analyzeButton" $ \el -> setCallback el OnClick analyzeFormula
  withElem "formulaInput" $ \el -> setCallback el OnKeyPress analyzeOnEnter

analyzeOnEnter keyCode = when (keyCode == 13) (analyzeFormula 0)

analyzeFormula _ = do
  formulaStr <- withElem "formulaInput" $ \el -> getProp el "value"

  case formula formulaStr of
    Left (pos, msg) -> alert $ "error at character " ++ show pos ++ ": " ++ msg
    Right x -> let normal     = mkNormal x
                   nnf        = mkNNF normal
                   cnf        = mkCNF nnf
                   scnf       = simplifyCNF cnf
                   dnf        = mkDNF nnf
                   sdnf       = simplifyDNF dnf
                   simplified = simplify x

               in do
                 withElem "cnfCode"        $ \el -> setProp el "innerHTML" $ prettyFormulaString cnf
                 withElem "scnfCode"       $ \el -> setProp el "innerHTML" $ prettyFormulaString scnf
                 withElem "dnfCode"        $ \el -> setProp el "innerHTML" $ prettyFormulaString dnf
                 withElem "sdnfCode"       $ \el -> setProp el "innerHTML" $ prettyFormulaString sdnf
                 withElem "nnfCode"        $ \el -> setProp el "innerHTML" $ prettyFormulaString nnf
                 withElem "simplifiedCode" $ \el -> setProp el "innerHTML" $ prettyFormulaString simplified