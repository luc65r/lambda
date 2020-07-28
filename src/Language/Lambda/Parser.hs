{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}

module Language.Lambda.Parser
    ( parse
    ) where

import Control.Applicative
import Data.Char (isDigit)

import Language.Lambda (Lambda(..))

newtype Parser a = P { unP :: String -> (String, Either String a) }

instance Functor Parser where
    fmap f (P st) = P $ \s -> case st s of
                                (r, Left a) -> (r, Left a)
                                (r, Right a) -> (r, Right (f a))

instance Applicative Parser where
    pure a = P $ \s -> (s, Right a)

    P ff <*> P xx = P $ \s0 -> case ff s0 of
                                 (s1, Left a) -> (s1, Left a)
                                 (s1, Right f) -> case xx s1 of
                                                   (s2, Left a) -> (s2, Left a)
                                                   (s2, Right x) -> (s2, Right (f x))

instance Alternative Parser where
    empty = P $ \s -> (s, Left "")

    (P f1) <|> (P f2) = P $ \s0 -> case f1 s0 of
                                     (_, Left _) -> f2 s0
                                     (s1, Right a) -> (s1, Right a)

    many (P f) = P go where
        go s0 = case f s0 of
                  (_, Left _) -> (s0, Right [])
                  (s1, Right a) -> case go s1 of
                                    (s2, Left b) -> (s2, Left b)
                                    (s2, Right as) -> (s2, Right (a:as))

    some (P f) = P $ \s0 -> case f s0 of
                              (s1, Left a) -> (s1, Left a)
                              (s1, Right a) ->
                                  let (P fm) = many (P f)
                                   in case fm s1 of
                                        (s2, Left b) -> (s2, Left b)
                                        (s2, Right as) -> (s2, Right (a:as))

instance Monad Parser where
    return = pure

    (P x) >>= f = P $ \s -> case x s of
                              (r, Left a) -> (r, Left a)
                              (r, Right a) ->
                                  let P fm = f a
                                   in fm r


parens :: Parser a -> Parser a
parens p = do
    satisfy (== '(')
    many space
    x <- p
    many space
    satisfy (== ')')
    return x

space :: Parser Char
space = satisfy (== ' ')

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \case
                   [] -> ([], Left "Unexpected end")
                   (c:cs)
                       | f c -> (cs, Right c)
                       | otherwise -> (cs, Left "Don't satisfy")

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = p >>= rest
    where rest x = do f <- op
                      y <- p
                      rest (f x y)
                      <|> return x


parseVar :: Parser Lambda
parseVar = Var . read <$> some (satisfy isDigit)

parseAbs :: Parser Lambda -> Parser Lambda
parseAbs p = const Abs <$> (satisfy (== 'λ') <|> satisfy (== '\\')) <*> p

parseApp :: Parser Lambda
parseApp = chainl (parens parseApp
    <|> parseAbs (many space >> parseApp)
    <|> parseVar) (some space >> pure App)

-- | Run the parser.
--   Lambda is represented by λ or \.
parse :: String -> Either String Lambda
parse s = case unP parseApp s of
            ([], Right a) -> Right a
            (_, Left a) -> Left a
            _ -> Left "Unmatched"
