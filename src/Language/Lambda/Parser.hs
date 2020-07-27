module Language.Lambda.Parser
    ( parse
    ) where

import Control.Applicative
import Data.Char (isDigit)

import Language.Lambda (Lambda(..))

newtype Parser a = P { unP :: String -> (String, Maybe a) }

instance Functor Parser where
    fmap f (P st) = P $ \s -> case st s of
                                (r, Nothing) -> (r, Nothing)
                                (r, Just a) -> (r, Just (f a))

instance Applicative Parser where
    pure a = P $ \s -> (s, Just a)

    P ff <*> P xx = P $ \s0 -> case ff s0 of
                                 (s1, Nothing) -> (s1, Nothing)
                                 (s1, Just f) -> case xx s1 of
                                                   (s2, Nothing) -> (s2, Nothing)
                                                   (s2, Just x) -> (s2, Just (f x))

instance Alternative Parser where
    empty = P $ \s -> (s, Nothing)

    (P f1) <|> (P f2) = P $ \s0 -> case f1 s0 of
                                     (s1, Nothing) -> f2 s0
                                     (s1, Just a) -> (s1, Just a)

    many (P f) = P go where
        go s0 = case f s0 of
                  (_, Nothing) -> (s0, Just [])
                  (s1, Just a) -> case go s1 of
                                    (s2, Nothing) -> (s2, Nothing)
                                    (s2, Just as) -> (s2, Just (a:as))

    some (P f) = P $ \s0 -> case f s0 of
                              (s1, Nothing) -> (s1, Nothing)
                              (s1, Just a) ->
                                  let (P fm) = many (P f)
                                   in case fm s1 of
                                        (s2, Nothing) -> (s2, Nothing)
                                        (s2, Just as) -> (s2, Just (a:as))

instance Monad Parser where
    return = pure

    (P x) >>= f = P $ \s -> case x s of
                              (r, Nothing) -> (r, Nothing)
                              (r, Just a) ->
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
satisfy f = P $ \s -> case s of
                        [] -> ([], Nothing)
                        (c:cs)
                            | f c -> (cs, Just c)
                            | otherwise -> (cs, Nothing)

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = p >>= rest
    where rest x = do f <- op
                      y <- p
                      rest (f x y)
                      <|> return x


parseVar :: Parser Lambda
parseVar = (Var . read) <$> some (satisfy isDigit)

parseAbs :: Parser Lambda -> Parser Lambda
parseAbs p = const Abs <$> (satisfy (== 'λ') <|> satisfy (== '\\')) <*> p

parseApp :: Parser Lambda
parseApp = chainl (parens parseApp
    <|> parseAbs (many space >> parseApp)
    <|> parseVar) (some space >> pure App)

-- | Run the parser.
--   Lambda is represented by λ or \.
parse :: String -> Maybe Lambda
parse s = case unP parseApp s of
            ([], a) -> a
            _ -> Nothing
