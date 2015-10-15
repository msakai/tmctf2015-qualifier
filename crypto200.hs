{-# LANGUAGE OverloadedStrings #-}
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16 -- from base16-bytestring package
import Data.Monoid
import Codec.Crypto.AES -- from AES package
import Control.Monad

msg :: BS.ByteString
msg = "The message is protected by AES!"

msg1 :: BS.ByteString
(msg1,msg2) = BS.splitAt 16 msg

cipher1_broken :: BS.ByteString
cipher1_broken = fst $ Base16.decode "fe000000000000000000000000009ec3"

cipher2 :: BS.ByteString
cipher2 = fst $ Base16.decode "307df037c689300bbf2812ff89bc0b49"

key :: BS.ByteString
key = head $ do
  k1 <- [0..255]
  k2 <- [0..255]
  let key = "5d6I9pfR7C1JQt" <> BS.pack [k1,k2]
  let p = crypt' CBC key cipher1_broken Decrypt cipher2
  guard $ BS.index p 0  == BS.index msg2 0
  guard $ BS.index p 14 == BS.index msg2 14
  guard $ BS.index p 15 == BS.index msg2 15
  return key

key' :: BS.ByteString
key' = head $ do
  k1 <- [0..255]
  k2 <- [0..255]
  let key = "5d6I9pfR7C1JQt" <> BS.pack [k1,k2]
  let c = crypt' CBC key cipher1_broken Encrypt msg2
  guard $ BS.index c 0  == BS.index cipher2 0
  guard $ BS.index c 14 == BS.index cipher2 14
  guard $ BS.index c 15 == BS.index cipher2 15
  return key
{-
こっちだとAES処理前のxor結果が期待通りにならないので、うまく行かない
*Main> Base16.encode $ crypt' CBC key cipher1_broken Encrypt msg2
"cc22e2e2810d2c92484dc4802ebd7ba2"
*Main> Base16.encode $ crypt' CBC key cipher1 Encrypt msg2
"307df037c689300bbf2812ff89bc0b49"
-}

cipher1 :: BS.ByteString
cipher1 = BS.pack $ BS.zipWith xor (crypt' ECB key (BS.replicate 16 0) Decrypt cipher2) msg2
{-
*Main> Base16.encode cipher1
"fe1199011d45c87d10e9e842c1949ec3"
-}

-- "Key:rVFvN9KLeYr6"
iv :: BS.ByteString
iv = BS.pack $ BS.zipWith xor (crypt' ECB key (BS.replicate 16 0) Decrypt cipher1) msg1
