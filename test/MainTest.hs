module Main where

import Test.Hspec
import qualified Data.ByteString as BS
import Lib (parsePacket)
import Data.Word (Word16)

main :: IO ()
main = hspec $ do
  describe "parsePacket" $ do
    it "parses a valid packet with ID and length" $ do
      let packet = BS.pack [0x01, 0x02, 0x00, 0x10] -- ID: 0x0102, Length: 16
      parsePacket packet `shouldBe` Just (0x0102, 16)

    it "returns Nothing for a packet with insufficient length" $ do
      let packet = BS.pack [0x01, 0x02] -- Too short
      parsePacket packet `shouldBe` Nothing

    it "parses a packet with maximum ID and length" $ do
      let packet = BS.pack [0xFF, 0xFF, 0xFF, 0xFF] -- ID: 0xFFFF, Length: 65535
      parsePacket packet `shouldBe` Just (0xFFFF, 65535)

    it "returns Nothing for an empty packet" $ do
      let packet = BS.empty
      parsePacket packet `shouldBe` Nothing