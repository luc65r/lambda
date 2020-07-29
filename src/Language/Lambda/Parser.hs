{- |
Module      : Language.Lambda.Parser
Copyright   : (c) Lucas Ransan, 2020
License     : MIT
Maintainer  : lucas@ransan.tk
Stability   : stable
Portability : portable

Definition of the functions 'parse' and 'parseLambda',
which parse a lambda expression from a string.
-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Lambda.Parser
    ( parseLambda
    , parse
    ) where

import Control.Applicative
import Data.Void
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Lambda (Lambda(..))

type Parser = M.Parsec Void String

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do { x <- p; rest x }
    where rest x = do
            { f <- op
            ; y <- p
            ; rest (f x y)
            } <|> return x

sc :: Parser ()
sc = L.space
    C.space1
    (L.skipLineComment ";")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser Lambda
parens = symbol "(" *> parseExpr <* symbol ")"

parseExpr :: Parser Lambda
parseExpr = parseApp <|> parseTerm

parseTerm :: Parser Lambda
parseTerm = M.choice
    [ parseVar
    , parseAbs
    , parens
    ]

parseVar :: Parser Lambda
parseVar = do
    n <- lexeme L.decimal
    if n > 0
       then pure $ Var n
       else fail "variable can't be 0"

parseAbs :: Parser Lambda
parseAbs = Abs <$> ((symbol "λ" <|> symbol "\\") *> parseExpr)

parseApp :: Parser Lambda
parseApp = chainl1 parseTerm $ pure App

-- | Run the parser.
--   Lambda can be represented by λ or \.
--   The parser accepts extra parentheses and spaces.
--   You will need to import 'Text.Megaparsec' to deal with the errors.
parseLambda :: String -> Either (M.ParseErrorBundle String Void) Lambda
parseLambda = M.parse (sc *> parseExpr <* M.eof) ""

-- | Run the parser.
--   The errors are transformed to 'String' for you,
--   so you don't have to import 'Text.Megaparsec'.
parse :: String -> Either String Lambda
parse s = case parseLambda s of
            Left e -> Left $ M.errorBundlePretty e
            Right l -> Right l
