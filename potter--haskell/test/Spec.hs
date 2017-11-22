import           Lib
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "price" $ do

    it "says none if no books bought" $ price [] `shouldBe` 0

    it "returns the price of 1 book if only 1 book selected" $
         price [0] `shouldBe` 8

