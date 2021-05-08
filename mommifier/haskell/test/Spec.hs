import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "mommifier" $ do

    it "returns an empty string as is" $
        mommify "" `shouldBe` ""
