import           Lib
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "dummy" $ do

    it "always passes" $
        1 `shouldBe` 1
