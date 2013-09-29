module Encoding where
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.ByteString (ByteString)

encodeUtf8 :: String -> ByteString
encodeUtf8 = T.encodeUtf8 . T.pack

decodeUtf8 :: ByteString -> String
decodeUtf8 = T.unpack . T.decodeUtf8
