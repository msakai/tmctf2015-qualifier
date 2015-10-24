{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ASN1.Encoding as ASN1 -- asn1-encoding package
import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Types as ASN1 -- asn1-types package
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64 -- base64-bytestring package
import Data.List (unfoldr)
import Data.Maybe (fromJust)
import qualified Data.PEM as PEM -- pem package
import Math.NumberTheory.Moduli -- arithmoi package
import qualified Codec.Crypto.RSA as RSA -- RSA package

{-
$ openssl rsa -pubin -in PublicKey.pem -text -noout
Public-Key: (256 bit)
Modulus:
   00:b6:2d:ce:9f:25:81:63:57:23:db:6b:18:8f:12:
   f0:46:9c:be:e0:cb:c5:da:cb:36:c3:6e:0c:96:b6:
   ea:7b:fc
Exponent: 65537 (0x10001)
$ ruby -e 'printf("0x%x\n", 0x00b62dce9f2581635723db6b188f12f0469cbee0cbc5dacb36c36e0c96b6ea7bfc | 1)'
0x0xb62dce9f2581635723db6b188f12f0469cbee0cbc5dacb36c36e0c96b6ea7bfd
$ msieve152 0xb62dce9f2581635723db6b188f12f0469cbee0cbc5dacb36c36e0c96b6ea7bfd
-}

n, p, q, e, d :: Integer
n = p*q
p = 279125332373073513017147096164124452877
q = 295214597363242917440342570226980714417
e = 65537
Just d = e `invertMod` ((p-1)*(q-1))

cipher :: BS.ByteString
Right cipher = Base64.decode "kPmDFLk5b/torG53sThWwEeNm0AIpEQek0rVG3vCttc="

-- (1) 手で復号する場合。(paddingが残る)

cipher' :: Integer
cipher' = BS.foldl (\ret b -> ret `shiftL` 8 .|. fromIntegral b) 0 cipher

plain_padded' :: Integer
plain_padded' = powerMod cipher' d n

plain_padded :: BS.ByteString
plain_padded = BS.pack $ reverse $ unfoldr phi plain_padded'
  where
    phi 0 = Nothing
    phi x = Just (fromIntegral (x .&. 0xff), x `shiftR` 8)

-- (2) RSAパッケージを使った場合。(paddingがちゃんと取り除かれる)

pub :: RSA.PublicKey
pub =
  RSA.PublicKey
  { RSA.public_size = 256 `div` 8 -- in bytes
  , RSA.public_n = n
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

plain :: BL.ByteString
plain = RSA.decryptPKCS priv $ BL.fromStrict cipher

-- (3) PEMファイルを生成してOpenSSLで復号する場合

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
ghci> BL.writeFile "crypto100.key" (exportRSAPrivateKey priv)
ghci> :quit
$ echo "kPmDFLk5b/torG53sThWwEeNm0AIpEQek0rVG3vCttc=" | base64 --decode | openssl rsautl -decrypt -inkey crypto100.key 
-}
