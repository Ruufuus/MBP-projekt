{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module UtilsCipherBlockECB(
    utilsDecryptECB,
    utilsEncryptECB
    )where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import           Crypto.Error (CryptoFailable(..), CryptoError(..))

import Crypto.Data.Padding (pad, unpad, Format(ZERO))
import qualified Crypto.Random.Types as CRT

import           Data.ByteArray (ByteArray)
import           Data.ByteString (ByteString)

import UtilsAES
        

encryptECB :: (BlockCipher c, ByteArray a) => Key c a -> a -> Either CryptoError a
encryptECB secretKey msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ecbEncrypt c msg

decryptECB :: (BlockCipher c, ByteArray a) => Key c a  -> a -> Either CryptoError a
decryptECB secretKey msg = 
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ecbDecrypt c msg

utilsEncryptECB :: ByteString -> Key AES256 ByteString -> IO ByteString
utilsEncryptECB msg secretKey = do
    let paddedMsg = pad (ZERO  blockLength) msg
    let encryptedMsg = encryptECB secretKey paddedMsg
    case encryptedMsg of
      Left err -> error $ show err
      Right eMsg -> do
        return eMsg


utilsDecryptECB :: ByteString -> Key AES256 ByteString  -> IO ByteString
utilsDecryptECB eMsg secretKey = do
    let paddedEMsg = pad (ZERO  blockLength) eMsg
    let decryptedMsg = decryptECB secretKey paddedEMsg
    case decryptedMsg of
      Left err -> error $ show err
      Right dMsg -> do
        return dMsg