import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "encode" $ do

    it "converts 1 to I" $
        encode 1 `shouldBe` "I"

    it "converts 2 to II" $
        encode 2 `shouldBe` "II"

    it "converts 4 to IV" $
        encode 4 `shouldBe` "IV"

    it "converts 5 to V" $
        encode 5 `shouldBe` "V"

    it "converts 6 to VI" $
        encode 6 `shouldBe` "VI"

    it "converts 8 to VIII" $
        encode 8 `shouldBe` "VIII"

    -- it "converts 9 to IX" $
    --     encode 9 `shouldBe` "IX"
