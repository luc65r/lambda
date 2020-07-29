module Language.Lambda.ParserSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec

import Language.Lambda
import Language.Lambda.Parser

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse variables" $ do
            parse "5" `shouldBe` Right (Var 5)

        it "should parse multi-digit variables" $ do
            parse "2134096" `shouldBe` Right (Var 2134096)

        it "should parse simple applications" $ do
            parse "3 4" `shouldBe` Right (App (Var 3) (Var 4))

        it "should parse simple applications with multi-digit variables" $ do
            parse "79 05237" `shouldBe` Right (App (Var 79) (Var 5237))

        it "should parse applications" $ do
            parse "6 12 1" `shouldBe` Right (apps [Var 6, Var 12, Var 1])

        it "should parse applications with extra spaces" $ do
            parse "1  2               3 90     8"
                `shouldBe` Right (apps [Var 1, Var 2, Var 3, Var 90, Var 8])

        it "should parse applications with leading and trailing spaces" $ do
            parse "    10 8          54 "
                `shouldBe` Right (apps [Var 10, Var 8, Var 54])

        it "should parse applications with parentheses" $ do
            parse "1 (22 333)"
                `shouldBe` Right (App (Var 1) (App (Var 22) (Var 333)))

        it "should parse applications with extra parentheses" $ do
            parse "(((     (6  ((90) ))    )        1 (  (7 4))))  "
                `shouldBe` Right (apps [Var 6, Var 90, Var 1, App (Var 7) (Var 4)])

        it "should parse abstractions with λ" $ do
            parse "λ 8" `shouldBe` Right (Abs (Var 8))

        it "should parse abstractions with \\" $ do
            parse "\\ 2" `shouldBe` Right (Abs (Var 2))

        it "should parse abstraction with both λ and \\" $ do
            parse " λλ\\ \\      λ3  " `shouldBe` Right (iterate Abs (Var 3) !! 5)

        it "should parse lambda expressions" $ do
            parse "  (λ ((λ 4   2 (λ ((1 ((3))  )  ))  ) )) (λ5  (1) )"
                `shouldBe` Right (apps [ Abs (Abs (apps [ Var 4, Var 2
                                                        , Abs (apps [Var 1, Var 3])]))
                                       , Abs (apps [Var 5, Var 1])])

        it "should not parse unmatched parentheses" $ do
            parse "(5))" `shouldSatisfy` isLeft
            parse "( (λ 9 )" `shouldSatisfy` isLeft
            parse "((  (( ((((1" `shouldSatisfy` isLeft
            parse "(((1)) ((\\ 72 )) 2))" `shouldSatisfy` isLeft
