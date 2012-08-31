{- |
Module      :  $Header$
Description :  Parse a string to a propositional Formula
Copyright   :  (c) Till Theis
License     :  MIT

Maintainer  :  Till Theis <theis.till@gmail.com>
Stability   :  experimental
Portability :  portable

Parse a string to a propositional Formula.

-}


module PropositionalLogic.Parser (formula) where

import PropositionalLogic.Logic

import Data.Char (toLower, isSpace, isAlpha)
import Data.Function (on)
import Data.List (isPrefixOf)
import Control.Monad (liftM, liftM2)

-- * Exports


-- | Translate a string to a 'Formula'. If it can't be translated the function
-- returns the position of the first error and a message describing the problem.
formula :: String -> Either (Int, String) (Formula Fancy)
formula s =
  case runParser tokenize s of
       Right ([], ts) -> case runParser parse ts of
                              Right ([], f)   -> Right f
                              Right (t:_, _)  -> Left (position t, "parser: input not exhausted")
                              Left ([], msg)  -> Left (-1, msg)
                              Left (t:_, msg) -> Left (position t, msg)
       Right (s', _)  -> Left (length s - length s', "tokenizer: input not exhausted")
       Left (s', msg) -> Left (length s - length s', "tokenizer: " ++ msg)


-- * Library Code
-- I had to create my own parsing library because UHC can't compile Cabal
-- packages to Javascript.
-- The following functions mostly resemble the the ones from Parsec
-- (<http://hackage.haskell.org/package/parsec>)


-- | The Parser type parameterized over the input stream element type @s@ and
-- the return type a. A parser takes the input and produces either an error
-- message together with the original input or a return value together with the
-- not yet consumed input.
newtype Parser s a = Parser { runParser :: [s] -> Either ([s], String) ([s], a) }

parserError s msg = Left (s, msg)
parserAccept s a = Right (s, a)

instance Monad (Parser s) where
  p >>= f = Parser $ \s ->
    case runParser p s of
      Right (s', a) -> runParser (f a) s'
      Left l -> Left l

  return = Parser . flip parserAccept
  fail = Parser . flip parserError


(<|>) :: Parser s a -> Parser s a -> Parser s a
p1 <|> p2 = Parser $ \s -> runParser p1 s `or` runParser p2 s
  where (Left _) `or` b = b
        a `or` _ = a

eof :: Parser s ()
eof = Parser go
  where go [] = parserAccept [] ()
        go s  = parserError s "eof"

many :: Parser s a -> Parser s [a]
many p = many1 p <|> return []

many1 :: Parser s a -> Parser s [a]
many1 p = liftM2 (:) p (many p)

many2 :: Parser s a -> Parser s [a]
many2 p = liftM2 (:) p (many1 p)

oneOf :: Eq s => [s] -> Parser s s
oneOf elems = satisfy (`elem` elems)

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser go
  where go (x:xs) | p x = parserAccept xs x
        go s = parserError s "not satisfied"

lookAhead :: Parser s a -> Parser s a
lookAhead p = Parser $ \s -> either (parserError s . snd) (parserAccept s . snd) (runParser p s)

choice :: [Parser s a] -> Parser s a
choice = foldr1 (<|>)


-- * Scanning


data TokenID = TTok | FTok | SpaceTok | LParTok | RParTok | SymTok | NotTok | AndTok | OrTok | ImplTok | EqTok | ErrTok deriving Show
data Token = Token { tid :: TokenID, position :: Position, match :: String } deriving (Show, Eq)
type Position = Int

instance Eq TokenID where
  TTok     == TTok     = True
  FTok     == FTok     = True
  SpaceTok == SpaceTok = True
  LParTok  == LParTok  = True
  RParTok  == RParTok  = True
  SymTok   == SymTok   = True
  NotTok   == NotTok   = True
  AndTok   == AndTok   = True
  OrTok    == OrTok    = True
  ImplTok  == ImplTok  = True
  EqTok    == EqTok    = True
  ErrTok   == ErrTok   = True
  _        == _        = False

tokenParser :: TokenID -> Parser Char String -> Parser Char Token
tokenParser t p = p >>= \match -> return $ Token t 0 match

tokenParsers =
  [ tokenParser SpaceTok $ many1 (satisfy isSpace)
  , tokenParser TTok     $ stringIgnoreCase "1"
  , tokenParser FTok     $ stringIgnoreCase "0"
  , tokenParser SymTok   $ many2 (satisfy isAlpha)
  , tokenParser SymTok   $ singleCharSymbol
  , tokenParser LParTok  $ stringIgnoreCase "("
  , tokenParser RParTok  $ stringIgnoreCase ")"
  , tokenParser NotTok   $ liftM return $ oneOf "!¬"
  , tokenParser AndTok   $ liftM return $ oneOf "^∧"
  , tokenParser OrTok    $ liftM return $ oneOf "v∨"
  , tokenParser EqTok    $ stringIgnoreCase "<->"
  , tokenParser ImplTok  $ stringIgnoreCase "->"
  , tokenParser ErrTok   $ liftM return (satisfy (const True))
  ]

singleCharSymbol = do
  match <- satisfy (\c -> isAlpha c && c /= 'v') -- v is "or" operator
  lookAhead $ (satisfy (not . isAlpha) >> return ()) <|> eof
  return $ match:""

tokenize :: Parser Char [Token]
tokenize = do
  rawTokens <- many1 $ choice tokenParsers
  eof
  return $ updatePositions rawTokens

updatePositions :: [Token] -> [Token]
updatePositions = go 0
  where go offs (Token tid _ match : ts) =
          Token tid offs match : go (offs + length match) ts
        go _ _ = []

stringIgnoreCase :: String -> Parser Char String
stringIgnoreCase str = Parser $ \input ->
  if str `isPrefixOfIgnoreCase` input
    then uncurry parserAccept $ swap $ splitAt (length str) input
    else parserError input str
  where
    isPrefixOfIgnoreCase = isPrefixOf `on` toLowerString
    toLowerString = map toLower
    swap (a, b) = (b, a)


-- * Parsing


type FParser = Parser Token (Formula Fancy)

token :: TokenID -> Parser Token Token
token t = satisfy $ (==) t . tid

parse :: Parser Token (Formula Fancy)
parse = Parser $ runParser pFormula . withoutSpaces
  where withoutSpaces = filter ((/=) SpaceTok . tid)

parens :: Parser Token a -> Parser Token a
parens p = do
  token LParTok
  a <- p
  token RParTok
  return a

andEOF :: Parser s a -> Parser s a
andEOF p = do
  a <- p
  eof
  return a


formulaParsers = [pConnective, pNegation, pTrue, pFalse, pSymbol]

pFormula :: FParser
pFormula = choice $ map andEOF formulaParsers

pSubFormula :: FParser
pSubFormula = choice [pTrue, pFalse, pSymbol, pNegation, choice $ map parens formulaParsers]

pTrue :: FParser
pTrue = token TTok >> return T

pFalse :: FParser
pFalse = token FTok >> return F

pSymbol :: FParser
pSymbol = token SymTok >>= return . Symbol . match

pNegation :: FParser
pNegation = token NotTok >> pSubFormula >>= return . Negation

associativeParsers =
  [ token AndTok  >> return Conjunction
  , token OrTok   >> return Disjunction
  ]
rightAssociativeParsers =
  [ token ImplTok >> return Implication
  , token EqTok   >> return Equivalence
  ]

pAssociative :: FParser
pAssociative = do
  f  <- pSubFormula
  op <- choice associativeParsers
  g  <- pAssociative <|> pSubFormula
  return $ f `op` g

pRightAssociative :: FParser
pRightAssociative = do
  f  <- pAssociative <|> pSubFormula
  op <- choice rightAssociativeParsers
  g  <- pRightAssociative <|> pAssociative <|> pSubFormula
  return $ f `op` g

pConnective :: FParser
pConnective = pRightAssociative <|> pAssociative <|> pSubFormula
