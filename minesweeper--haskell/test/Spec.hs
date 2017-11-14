import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "fillNumbers" $ do

    it "return * for bomb tile" $
        fillNumbers ["*"] `shouldBe` ["*"]

    it "return 0 if no bombs around" $
        fillNumbers ["."] `shouldBe` ["0"]

    it "return 1 if the tile has 1 bomb right to it" $
        fillNumbers [".*"] `shouldBe` ["1*"]

    it "fill numbers with surrounded by bombs" $
        let input = ["***",
                     "*.*",
                     "***"]
            expected = ["***",
                        "*8*",
                        "***"]
        in fillNumbers input `shouldBe` expected

    it "fill numbers for simple board" $
        let input = ["*...",
                     "....",
                     ".*..",
                     "...."]
            expected = [
                      "*100",
                      "2210",
                      "1*10",
                      "1110"]
        in fillNumbers input `shouldBe` expected

    it "fill numbers for non-square board" $
        let input = ["**...",
                     ".....",
                     ".*..."]
            expected = ["**100",
                        "33200",
                        "1*100"]
        in fillNumbers input `shouldBe` expected
