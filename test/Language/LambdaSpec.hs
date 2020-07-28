module Language.LambdaSpec (spec) where

import Test.Hspec
import Language.Lambda
import Language.Lambda.Parser

spec :: Spec
spec = do
    describe "apps" $ do
        it "should just work" $ do
            apps [Var 5] `shouldBe` Var 5
            apps [Var 2, Var 26] `shouldBe` App (Var 2) (Var 26)
            apps [Var 8, Var 7, Var 6, Var 5]
                `shouldBe` App (App (App (Var 8) (Var 7)) (Var 6)) (Var 5)
            apps [ Abs (Abs (apps [Var 4, Var 2, Abs (apps [Var 1, Var 3])]))
                 , Abs (apps [Var 5, Var 1])]
                `shouldBe` App (Abs (Abs (App (App (Var 4) (Var 2)) (Abs (App (Var 1) (Var 3))))))
                               (Abs (App (Var 5) (Var 1)))

    describe "reduct" $ do
        it "should reduct (λ 1) 1 to 1" $ do
            reduct (App (Abs (Var 1)) (Var 1)) `shouldBe` Var 1

        it "should reduct (λ 1 1) (λ 1 1) to (λ 1 1) (λ 1 1)" $ do
            reduct (App (Abs (App (Var 1) (Var 1))) (Abs (App (Var 1) (Var 1))))
                `shouldBe` App (Abs (App (Var 1) (Var 1))) (Abs (App (Var 1) (Var 1)))

        it "should reduct (λ λ 3 2) 2 to λ 2 3" $ do
            reduct (App (Abs (Abs (App (Var 3) (Var 2)))) (Var 2))
                `shouldBe` Abs (App (Var 2) (Var 3))


    describe "reductMax" $ do
        it "should reduct (λ λ 4 2 (λ 1 3)) (λ 5 1) to λ 3 (λ 6 1) (λ 1 (λ 7 1))" $ do
            reductMax <$> parse "(λ λ 4 2 (λ 1 3)) (λ 5 1)"
                `shouldBe` parse "λ 3 (λ 6 1) (λ 1 (λ 7 1))"

        it "1 + 1 should be 2" $ do
            (do p <- plus
                o <- intToLambda 1
                lambdaToInt . reductMax $ App (App p o) o)
                `shouldBe` Right 2

        it "4^2 - 2 * 4 should be 8" $ do
            (do p <- pow
                s <- sub
                m <- mult
                t <- intToLambda 2
                f <- intToLambda 4
                lambdaToInt . reductMax $ App (App s (App (App p f) t)) (App (App m t) f))
                `shouldBe` Right 8


intToLambda :: Int -> Either String Lambda
intToLambda n
    | n < 0 = Left "Invalid number"
    | otherwise = Right (Abs (Abs a))
    where a = iterate (App (Var 2)) (Var 1) !! n

lambdaToInt :: Lambda -> Either String Int
lambdaToInt (Abs (Abs a)) = depth a
    where depth x = case x of Var 1 -> Right 0
                              App (Var 2) b -> (+ 1) <$> depth b
                              _ -> Left "Lambda expression doesn't correspond to a Church numeral"
lambdaToInt _ = Left "Lambda expression doesn't correspond to a Church numeral"

succ', plus, mult, pow, pred', sub :: Either String Lambda

succ' = reductMax <$> parse "λλλ 2 (3 2 1)"

plus = do
    s <- succ'
    return $ reductMax (Abs (Abs (App (App (Var 2) s) (Var 2))))

mult = do
    p <- plus
    z <- intToLambda 0
    return $ reductMax (Abs (Abs (App (App (Var 2) (App p (Var 1))) z)))

pow = reductMax <$> parse "λλ 1 2"

pred' = reductMax <$> parse "λλλ 3 (λλ 1 (2 4)) (λ 2) (λ 1)"

sub = do
    p <- pred'
    return $ reductMax (Abs (Abs (App (App (Var 1) p) (Var 2))))
