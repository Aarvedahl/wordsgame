import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "joins up a grid into a string" $ do
      formatGrid ["abc", "def", "ghi"] `shouldBe` "abc\ndef\nghi\n"

  describe "findWords" $ do
    it "should find all words that exists on the Grid" $ do
      findWords grid languages `shouldBe` languages

    it "Should not find words that do not exist on the Grid" $ do
       findWords grid ["abc",  "bca"] `shouldBe` []
