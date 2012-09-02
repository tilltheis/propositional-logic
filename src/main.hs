import PropositionalLogic
import System.Environment

main = do
  args <- getArgs
  case formula $ args !! 0 of
    Left (pos, msg) -> putStrLn $ "error at character " ++ show pos ++ ": " ++ msg
    Right x         -> putStrLn $ prettyFormulaString $ simplify x
