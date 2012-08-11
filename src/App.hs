import Control.Monad

import PropositionalLogic

import Language.UHC.JScript.Prelude
import Language.UHC.JScript.W3C.HTML5 hiding (documentGetElementById)
import qualified Language.UHC.JScript.W3C.HTML5 as HTML5 (documentGetElementById)
import Language.UHC.JScript.Assorted
import Language.UHC.JScript.ECMA.Expressions

main = do
  win <- window
  onLoadAction <- wrapIO onLoad
  setAttr "onload" onLoadAction win

onLoad :: IO ()
onLoad = do
  doc <- document

  analyzeButton <- documentGetElementById doc "analyzeButton"
  onClickAction <- wrapIO analyzeFormula
  setAttr "onclick" onClickAction analyzeButton

  formulaInput <- documentGetElementById doc "formulaInput"
  onKeyPressAction <- wrapIO1 analyzeOnEnter
  setAttr "onkeypress" onKeyPressAction formulaInput

  return ()

analyzeOnEnter :: Event -> IO ()
analyzeOnEnter e = when (eventKeyCode e == 13) analyzeFormula

analyzeFormula :: IO ()
analyzeFormula = do
  doc <- document
  formulaInput <- documentGetElementById doc "formulaInput"
  formulaStr <- jsStringToString `liftM` getAttr "value" formulaInput

  case formula formulaStr of
    Right x -> let normal     = mkNormal x
                   nnf        = mkNNF normal
                   cnf        = mkCNF nnf
                   scnf       = simplifyCNF cnf
                   dnf        = mkDNF nnf
                   sdnf       = simplifyDNF dnf
                   simplified = simplify x

               in do
                 cnfCode <- documentGetElementById doc "cnfCode"
                 setAttr "innerHTML" (stringToJSString $ prettyFormulaString cnf) cnfCode

                 scnfCode <- documentGetElementById doc "scnfCode"
                 setAttr "innerHTML" (stringToJSString $ prettyFormulaString scnf) scnfCode

                 dnfCode <- documentGetElementById doc "dnfCode"
                 setAttr "innerHTML" (stringToJSString $ prettyFormulaString dnf) dnfCode

                 sdnfCode <- documentGetElementById doc "sdnfCode"
                 setAttr "innerHTML" (stringToJSString $ prettyFormulaString sdnf) sdnfCode

                 nnfCode <- documentGetElementById doc "nnfCode"
                 setAttr "innerHTML" (stringToJSString $ prettyFormulaString nnf) nnfCode

                 simplifiedCode <- documentGetElementById doc "simplifiedCode"
                 setAttr "innerHTML" (stringToJSString $ prettyFormulaString simplified) simplifiedCode

                 return ()

    Left (pos, msg) -> alert $ "error at character " ++ show pos ++ ": " ++ msg

data WindowPtr
type Window = JSPtr WindowPtr

foreign import js "window"
  window :: IO Window

documentGetElementById d = HTML5.documentGetElementById d . stringToJSString

foreign import js "wrapper"
  wrapIO1 :: (a -> IO ()) -> IO (JSFunPtr (a -> IO ()))

data EventPtr
type Event = JSPtr EventPtr

foreign import js "%1.keyCode"
  eventKeyCode :: Event -> Int