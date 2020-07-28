{- |
Module      : Language.Lambda
Copyright   : (c) Lucas Ransan, 2020
License     : MIT
Maintainer  : lucas@ransan.tk
Stability   : stable
Portability : portable

Definition of the type 'Lambda', which represents lambda terms using De Bruijn indices,
and definition of the functions to perform a β-reduction.
-}

module Language.Lambda
    ( Lambda(..)
    , apps
    , reduct
    , reductible
    , reductMax
    , reductMax'
    ) where

-- | A lambda term can be:
data Lambda
    -- | A variable, represented by an integer;
    = Var Int
    -- | An abstaction (λ M), its body extends as far right as possible,
    --   so λ λ M means λ (λ M);
    | Abs Lambda
    -- | An application (M N), which is left-associative,
    --   so M N O means (M N) O.
    | App Lambda Lambda
    deriving (Eq, Read)

instance Show Lambda where
    showsPrec _ (Var n) = shows n
    showsPrec n (Abs a) = showParen (n > 9) $ showString "λ " . showsPrec 9 a
    showsPrec n (App a b) = showParen (n > 10) $ showsPrec 10 a . showString " " . showsPrec 11 b


-- | Chain multiple 'App'
apps :: [Lambda] -> Lambda
apps = foldl1 App

-- | Perform one β-reduction
reduct :: Lambda -> Lambda
reduct (App (Abs a) b) = substitute a 1 b
reduct (App a b) = App (reduct a) (reduct b)
reduct (Abs a) = Abs $ reduct a
reduct (Var a) = Var a

-- | Reduct the lambda expression until it reducts to itself.
--   reductMax ((λ 1 1) (λ 1 1)) will stop after the first reduction.
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

-- | Determine if the lambda expression can be reducted.
--   (λ 1 1) (λ 1 1) can be reducted, even though it reducts to itself.
reductible :: Lambda -> Bool
reductible (App (Abs _) _) = True
reductible (App a b) = reductible a || reductible b
reductible (Abs a) = reductible a
reductible (Var _) = False

-- | Reduct the lambda expression until it can't be reducted
--   reductMax' ((λ 1 1) (λ 1 1)) won't stop
reductMax' :: Lambda -> Lambda
reductMax' = until (not . reductible) reduct
