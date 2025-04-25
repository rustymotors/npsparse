module Main where

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Network.Socket
import Network.Socket.ByteString
import Numeric (showHex)

-- | Convert ByteString to a hex string
bytesToHex :: BS.ByteString -> String
bytesToHex = concatMap (\b -> let h = showHex b "" in if length h == 1 then '0' : h else h) . BS.unpack

-- | Parse the packet to extract the ID and length
parsePacket :: BS.ByteString -> Maybe (Word16, Word16)
parsePacket bs
  | BS.length bs < 4 = Nothing
  | otherwise =
      let packetId = (fromIntegral (BS.index bs 0) `shiftL` 8) .|. fromIntegral (BS.index bs 1)
          packetLen = (fromIntegral (BS.index bs 2) `shiftL` 8) .|. fromIntegral (BS.index bs 3)
       in Just (packetId, packetLen)

-- | Handle a single client connection
handleClient :: Socket -> PortNumber -> IO ()
handleClient conn port = do
  msg <- recv conn 1024
  putStrLn $ "Received packet on port " ++ show port ++ ": " ++ bytesToHex msg
  case parsePacket msg of
    Just (packetId, packetLen) ->
      putStrLn $ "Packet ID: " ++ show packetId ++ ", Length: " ++ show packetLen
    Nothing ->
      putStrLn "Invalid packet received"
  close conn

-- | Start a server on a specific port
startServer :: PortNumber -> IO ()
startServer port = withSocketsDo $ do
  addr <- resolve
  sock <- createSocket addr
  putStrLn $ "Listening on port " ++ show port
  forever $ do
    (conn, _) <- accept sock
    void $ forkFinally (handleClient conn port) (const $ close conn)
  where
    resolve = do
      let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) Nothing (Just $ show port)
    createSocket addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      listen sock 10
      return sock

main :: IO ()
main = do
  let ports = [3000, 9226, 8227, 8228, 43300] -- List of ports to listen on
  mapM_ (forkIO . startServer) ports
  -- Keep the main thread alive indefinitely
  threadDelay maxBound
