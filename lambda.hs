data Lambda = Var Int | Abs Lambda | App Lambda Lambda
    deriving (Eq)

instance Show Lambda where
    showsPrec _ (Var n) = shows n
    showsPrec _ (Abs a) = showString "λ " . shows a
    showsPrec n (App a b) = showParen (n > 10) (showsPrec 10 a . showString " " . showsPrec 11 b)


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
