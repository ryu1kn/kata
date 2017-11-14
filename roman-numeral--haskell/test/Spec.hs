import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "encode" $ do

    it "converts 1 to I" $
        encode 1 `shouldBe` "I"
