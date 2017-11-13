import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "fillNumbers" $ do

    it "returns simplest resolved board" $
        fillNumbers [['*']] `shouldBe` [['*']]
