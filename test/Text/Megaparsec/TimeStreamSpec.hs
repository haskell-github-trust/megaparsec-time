module Text.Megaparsec.TimeStreamSpec (
  spec,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (DayOfWeek (..), TimeOfDay (..), fromGregorian)
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)
import qualified Text.Megaparsec.Time.Byte as Byte
import qualified Text.Megaparsec.Time.Char as Char

parseText :: Parsec Void Text a -> Text -> Either (ParseErrorBundle Text Void) a
parseText p = runParser p "test"

parseBytes
  :: Parsec Void ByteString a
  -> ByteString
  -> Either (ParseErrorBundle ByteString Void) a
parseBytes p = runParser p "test"

spec :: Spec
spec = do
  describe "Char parsers on Text" $ do
    it "dateParser" $
      parseText Char.dateParser (T.pack "Tue 11:03")
        `shouldBe` Right (Just (Right Tuesday), TimeOfDay 11 3 0)

    it "durationParser" $
      parseText Char.durationParser (T.pack "1h 2m 3s") `shouldBe` Right 3723

    it "gregorianDayParser" $
      parseText Char.gregorianDayParser (T.pack "25/10/2026")
        `shouldBe` Right (fromGregorian 2026 10 25)

  describe "Byte parsers on ByteString" $ do
    it "dateParser" $ do
      parseBytes Byte.dateParser (B.pack "Tue 11:03")
        `shouldBe` Right (Just (Right Tuesday), TimeOfDay 11 3 0)
      parseBytes Byte.dateParser (B.pack "-2 11:03")
        `shouldBe` Right (Just (Left (-2)), TimeOfDay 11 3 0)

    it "durationParser" $
      parseBytes Byte.durationParser (B.pack "1h 2m 3s") `shouldBe` Right 3723

    it "gregorianDayParser" $
      parseBytes Byte.gregorianDayParser (B.pack "2026-10-25")
        `shouldBe` Right (fromGregorian 2026 10 25)
