import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "score" $ do

    it "calculates score for a gutter game" $
        score [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] `shouldBe` 0

    it "calculates score for a all one game" $
        score [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] `shouldBe` 20

    it "calculates score for one spare game" $
        score [5, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] `shouldBe` 16

    it "calculates score for two spares game" $
        score [5, 5, 3, 7, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] `shouldBe` 27

    it "calculates score for one strike game" $
        score [10, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] `shouldBe` 24

    it "calculates score for a perfect game" $
        score [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10] `shouldBe` 300
