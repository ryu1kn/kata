import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "decompose" $ do

    it "tells a password itself should be accepted" $
        decompose "password" ["password"] `shouldBe` Just ["password"]
