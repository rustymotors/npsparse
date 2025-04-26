module Lib (parsePacket) where

import qualified Data.ByteString as BS
import Data.Word (Word16)
import Data.Bits (shiftL, (.|.))

-- | Parse the packet to extract the ID and length
parsePacket :: BS.ByteString -> Maybe (Word16, Word16)
parsePacket bs
  | BS.length bs < 4 = Nothing
  | otherwise =
      let packetId = (fromIntegral (BS.index bs 0) `shiftL` 8) .|. fromIntegral (BS.index bs 1)
          packetLen = (fromIntegral (BS.index bs 2) `shiftL` 8) .|. fromIntegral (BS.index bs 3)
      in Just (packetId, packetLen)