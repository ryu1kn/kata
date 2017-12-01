import           Lib
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "price" $ do

    it "says none if no books bought" $ price [] `shouldBe` 0

    it "returns the price of 1 book if only 1 book selected" $
        price [0] `shouldBe` 8

    it "returns the price of 2 books if they are the same" $
        price [0, 0] `shouldBe` 16

    it "applies discount if bought 2 different books" $
        price [0, 1] `shouldBe` 8 * 2 * (1 - 0.05)

    -- it "be applied if bought 3 different books" $
    --     price [0, 1, 2] `shouldBe` 8 * 3 * (1 - 0.1)

    -- it "not be applied if bought 3 same books" $
    --     price [0, 0, 0] `shouldBe` 8 * 3

    -- it "be applied only for 2 out of 3 books if only 2 books are different" $
    --     price [0, 1, 0] `shouldBe` 8 * 2 * (1 - 0.05) + 8

    -- it "be applied if bought 4 different books" $
    --     price [0, 1, 2, 3] `shouldBe` 8 * 4 * (1 - 0.2)

    -- it "not be applied if bought 4 same books" $
    --     price [0, 0, 0, 0] `shouldBe` 8 * 4

    -- it "be applied only for 3 out of 4 books if only 3 books are different" $
    --     price [0, 1, 0, 2] `shouldBe` 8 * 3 * (1 - 0.1) + 8

    -- it "be applied for 2 sets of 2 different books" $
    --     price [0, 1, 0, 1] `shouldBe` 8 * 2 * (1 - 0.05) * 2

    -- it "be applied for only 2 different books of 4" $
    --     price [0, 1, 0, 0] `shouldBe` 8 * 2 * (1 - 0.05) + 8 * 2

    -- it "be applied for 5 different books" $
    --     price [0, 1, 2, 3, 4] `shouldBe` 8 * 5 * (1 - 0.25)

    -- it "not be applied if bought 5 same books" $
    --     price [0, 0, 0, 0, 0] `shouldBe` 8 * 5

    -- it "be applied for 2 and 3 different books" $
    --     price [0, 1, 0, 1, 2] `shouldBe` 8 * 2 * (1 - 0.05) + 8 * 3 * (1 - 0.1)
