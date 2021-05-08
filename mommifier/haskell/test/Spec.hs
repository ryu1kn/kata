import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "mommifier" $ do

    it "returns an empty string as is" $
        mommify "" `shouldBe` ""

    it "returns 'mommy' if 'a' is given" $
        mommify "a" `shouldBe` "mommy"

    it "returns 'mommy' if 'e' is given" $
        mommify "e" `shouldBe` "mommy"

    it "returns 'mommy' if 'i' is given" $
        mommify "i" `shouldBe` "mommy"

    it "returns 'mommy' if 'o' is given" $
        mommify "o" `shouldBe` "mommy"

    it "returns 'mommy' if 'u' is given" $
        mommify "u" `shouldBe` "mommy"

    it "replaces to mommy only for a vowel" $
        mommify "ba" `shouldBe` "bmommy"
