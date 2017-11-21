import           Lib
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "price" $ do

    it "says none if no books bought" $ price ([]) `shouldBe` 0
