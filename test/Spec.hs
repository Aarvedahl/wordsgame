import Lib
import Test.Hspec
--import Test.QuickCheck
--import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "joins up a grid into a string" $ do
      formatGrid ["abc", "def", "ghi"] `shouldBe` "abc\ndef\nghi\n"

