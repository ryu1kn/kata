import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "decompose" $ do

    it "tells a password itself should be accepted" $
        decompose "password" ["password"] `shouldBe` Just ["password"]

    it "uses the 2nd password to build login attempt string" $
        decompose "password2" ["password1", "password2"] `shouldBe` Just ["password2"]

    it "tells if login attempt cannot be built from the passwords" $
        decompose "attempt" ["password"] `shouldBe` Nothing

    it "tells if login attempt string can be built from 2 passwords" $
        decompose "attempt" ["att", "empt"] `shouldBe` Just ["att", "empt"]

    it "works even if 2 passwords are given in reverse order" $
        decompose "attempt" ["empt", "att"] `shouldBe` Just ["att", "empt"]

    it "works even if 3 passwords are given in neither correct nor reverse order" $
        decompose "attempt" ["at", "pt", "tem"] `shouldBe` Just ["at", "tem", "pt"]

    it "uses the same password multiple times if necessary" $
        decompose "pass" ["pa", "s"] `shouldBe` Just ["pa", "s", "s"]

    it "passes sample test #1" $
        decompose "wedowhatwemustbecausewecan" ["because", "can", "do", "must", "we", "what"]
            `shouldBe` Just ["we", "do", "what", "we", "must", "because", "we", "can"]

    it "passes sample test #2" $
        decompose "helloworld" ["hello", "planet"] `shouldBe` Nothing

    it "passes sample test #3" $
        decompose "abcd" ["ab", "abcd", "cd"] `shouldBe` Just ["ab", "cd"]

    it "passes sample test #4" $
        decompose "zfzahm" ["ozkxyhkcst", "xvglh", "hpdnb", "zfzahm"] `shouldBe` Just ["zfzahm"]

    it "passes sample test #5" $
        decompose "gurwgrb" ["gurwgrb", "maqz", "holpkhqx", "aowypvopum"] `shouldBe` Just ["gurwgrb"]

    it "passes sample test #6" $
        decompose "aaaaaaaaaab" ["a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa", "aaaaaaa", "aaaaaaaa", "aaaaaaaaa", "aaaaaaaaaa"] `shouldBe` Nothing
