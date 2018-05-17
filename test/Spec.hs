import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "joins up a grid into a string" $ do
      (formatGrid (gridWithCoords ["abc", "def", "ghi"])) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "Should find words that exist on the Grid" $ do
      findWord grid "Haskell" `shouldBe` just "HASKELL"

  describe "findWords" $ do
    it "should find all words that exists on the Grid" $ do
      findWords grid languages `shouldBe` languages

    it "Should not find words that do not exist on the Grid" $ do
       findWords grid ["abc",  "bca"] `shouldBe` []

    it "Should find words that both exists and doesn't exist on the Grid" $ do
      findWords grid ["PHP", "abc",  "PYTHON", "bca"] `shouldBe` ["PHP", "PYTHON"]

    it "Should indent lines correctly" $ do
      skew ["abc", "abc", "abc"] `shouldBe` ["abc", "_abc", "__abc"]
