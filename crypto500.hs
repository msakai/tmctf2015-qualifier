{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import Data.String
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64 -- from base64-bytestring package
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import qualified Crypto.Hash.SHA1 as SHA1 -- from cryptohash package

chars1 :: Set Char
chars1 = Set.unions $ do
  s <- do
    let alpha = ['a'..'z'] ++ ['A'..'Z']
    a <- alpha
    msum
      [ return $ fromString [a]
      , do b <- alpha
           msum
             [ return $ fromString [a,b]
             , do c <- alpha
                  return $ fromString [a,b,c]
             ]
      ]
  return $ Set.fromList $ BS.unpack $ Base64.encode s

chars2 :: Set Char
chars2 = Set.fromList (['a'..'z']++['A'..'Z']++['0'..'9']++"+/=") `Set.difference` chars1

ans :: BS.ByteString
ans = "TMCTF{" <> (Base16.encode $ SHA1.hash $ BS.pack $ Set.toList chars2) <> "}"
