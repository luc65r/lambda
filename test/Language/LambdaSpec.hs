module Language.LambdaSpec (spec) where

import Test.Hspec
import Language.Lambda
import Language.Lambda.Parser

spec :: Spec
spec = do
    describe "reduct" $ do
        it "should reduct (λ 1) 1 to 1" $ do
            reduct (App (Abs (Var 1)) (Var 1)) `shouldBe` Var 1

        it "should reduct (λ 1 1) (λ 1 1) to (λ 1 1) (λ 1 1)" $ do
            reduct (App (Abs (App (Var 1) (Var 1))) (Abs (App (Var 1) (Var 1))))
                `shouldBe` (App (Abs (App (Var 1) (Var 1))) (Abs (App (Var 1) (Var 1))))

    describe "reductMax" $ do
        it "should reduct (λ λ 4 2 (λ 1 3)) (λ 5 1) to λ 3 (λ 6 1) (λ 1 (λ 7 1))" $ do
            reductMax <$> parse "(λ λ 4 2 (λ 1 3)) (λ 5 1)"
                `shouldBe` parse "λ 3 (λ 6 1) (λ 1 (λ 7 1))"
