{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ASN1.Encoding as ASN1 -- asn1-encoding package
import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Types as ASN1 -- asn1-types package
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64 -- base64-bytestring package
import Data.List (unfoldr)
import qualified Data.List.Split as Split
import Data.Maybe (fromJust)
import qualified Data.PEM as PEM -- pem package
import Math.NumberTheory.Moduli -- arithmoi package
import qualified Codec.Crypto.RSA as RSA -- RSA package

{-
$ openssl x509 -in metadata1.pem -text
$ openssl x509 -in metadata2.pem -text
-}
n1, n2 :: Integer
n1 = 0x00d64594936f4fdfb5dabcaef2f54b9040b9a6743e8142084164e32dc35e15a954981f3fc6af88b261213ffb3f52a0fddacea2ae58d8c25f7d65c3004ad061b1bef8b4bca703408d9f86fd2ea66db0e98ad2cb8cdb2d2122608f416072aa1d37ec4f648134472030bfdf2f08c6c5728b63c438155aed8ddb85f3ff9914ada9c239d39830271261e2c1e80c926828b742e4cad929702fe37810fa37b1b71d2a4c44b95bd74981fc6ae49f84823192a174383f1bdfe665a567f8b2989edef23c3a27aced93ac0a32c4a930ebc693a53e78be2ce3247d94c630be0489dbc795cc866ebf7ff9c1904b69d8dafe255043653b575dcbb311d254aab0020425583d073dbb
n2 = 0x00afffe033e18b02cd5d58d9295fd0f24a6a3edee7bf8ae4d582501f504bb7f0186b84ae944d12103eba5e553ab70437f70608aad518c5fc21df9072278cf64d94fdbca9e68a2595cc80daedcf727b4f144dab39032d6187ca0065fcec8835cb1149139ab0d0040b586f86d30cf14799882c6d1c811f79e0a6ee07faeb3f3563e7bdba8123e92b495cbeec2efbfe1070ff6bef6ea3cce027f4ea471b397dacb407b92da0e1c63a1f6ad146d66f2fa6d233e0b2d0cf747d2126d911a5c9b8c3f1818a3907cd4f28c1a61027097bd0cc6361e9fd6f66c0a63416aa7485a54c04dcc2a6a8baeef2040ca699b61be2dd63d7221674f81233efb06f866cca57e960a75d

p = gcd n1 n2

q = n2 `div` p

e = 65537

Just d = e `invertMod` ((p-1)*(q-1))

pub :: RSA.PublicKey
pub =
  RSA.PublicKey
  { RSA.public_size = 2048 `div` 8 -- in bytes
  , RSA.public_n = n2
  , RSA.public_e = e
  }

priv :: RSA.PrivateKey
priv =
  RSA.PrivateKey
  { RSA.private_pub = pub
  , RSA.private_d = d
  , RSA.private_p = p
  , RSA.private_q = q
  , RSA.private_dP = d `mod` (p-1)
  , RSA.private_dQ = d `mod` (q-1)
  , RSA.private_qinv = fromJust $ q `invertMod` p
  }

exportRSAPrivateKey :: RSA.PrivateKey -> BL.ByteString
exportRSAPrivateKey key = PEM.pemWriteLBS pem
  where
    pem =
      PEM.PEM
      { PEM.pemName = "RSA PRIVATE KEY"
      , PEM.pemHeader = []
      , PEM.pemContent = asn1
      }
    asn1 = ASN1.encodeASN1' ASN1.DER $ [ASN1.Start ASN1.Sequence] ++ map ASN1.IntVal xs ++ [ASN1.End ASN1.Sequence]
      where
        pub = RSA.private_pub key
        xs = [ 0
             , RSA.public_n pub
             , RSA.public_e pub
             , RSA.private_d key
             , RSA.private_p key
             , RSA.private_q key
             , RSA.private_dP key
             , RSA.private_dQ key
             , RSA.private_qinv key
             ]

{-
ghci> BL.writeFile "metadata2.key" (exportRSAPrivateKey priv)
ghci> :quit
-}
