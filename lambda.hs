import Control.Applicative
import Data.Char

data Lambda = Var Int | Abs Lambda | App Lambda Lambda
    deriving (Eq, Read)

instance Show Lambda where
    showsPrec _ (Var n) = shows n
    showsPrec n (Abs a) = showParen (n > 9) $ showString "λ " . showsPrec 9 a
    showsPrec n (App a b) = showParen (n > 10) $ showsPrec 10 a . showString " " . showsPrec 11 b

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
parseAbs p = const Abs <$> satisfy (== 'λ') <*> p

parseApp :: Parser Lambda
parseApp = chainl (parens parseApp
    <|> parseAbs (many space >> parseApp)
    <|> parseVar) (some space >> pure App)

parse :: String -> Maybe Lambda
parse s = case unP parseApp s of
            ([], a) -> a
            _ -> Nothing


reduct :: Lambda -> Lambda
reduct (App (Abs a) b) = substitute a 1 b
reduct (App a b) = App (reduct a) (reduct b)
reduct (Abs a) = Abs $ reduct a
reduct (Var a) = Var a

reductMax :: Lambda -> Lambda
reductMax x
    | x == y = x
    | otherwise = reductMax y
    where y = reduct x

substitute :: Lambda -> Int -> Lambda -> Lambda
substitute (App a b) binders x = App (substitute a binders x) (substitute b binders x)
substitute (Abs a) binders x = Abs $ substitute a (binders + 1) x
substitute (Var a) binders x
    | a == binders = incFreeVar x 0 $ binders - 1
    | a > binders = Var $ a - 1
    | otherwise = Var a

incFreeVar :: Lambda -> Int -> Int -> Lambda
incFreeVar (App a b) binders n = App (incFreeVar a binders n) (incFreeVar b binders n)
incFreeVar (Abs a) binders n = Abs $ incFreeVar a (binders + 1) n
incFreeVar (Var a) binders n
    | a > binders = Var $ a + n
    | otherwise = Var a

reductible :: Lambda -> Bool
reductible (App (Abs _) _) = True
reductible (App a b) = reductible a || reductible b
reductible (Abs a) = reductible a
reductible (Var _) = False

reductMax' :: Lambda -> Lambda
reductMax' = until (not . reductible) reduct

intToLambda :: Int -> Maybe Lambda
intToLambda n
    | n < 0 = Nothing
    | otherwise = Just (Abs (Abs a))
    where a = iterate (App (Var 2)) (Var 1) !! n

lambdaToInt :: Lambda -> Maybe Int
lambdaToInt (Abs (Abs a)) = depth a
    where depth x = case x of Var 1 -> Just 0
                              App (Var 2) b -> fmap (+ 1) $ depth b
                              _ -> Nothing
lambdaToInt _ = Nothing
