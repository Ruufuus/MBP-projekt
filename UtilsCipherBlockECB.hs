{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module UtilsCipherBlockECB(
    utilsDecryptECB,
    utilsEncryptECB
    )where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..))
import           Crypto.Error ( CryptoError(..))

import Crypto.Data.Padding (pad, Format(ZERO))

import           Data.ByteArray (ByteArray)

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

utilsEncryptECB :: (ByteArray b) => b -> Key AES256 b -> IO b
utilsEncryptECB msg secretKey = do
    let paddedMsg = pad (ZERO  blockLength) msg
    let encryptedMsg = encryptECB secretKey paddedMsg
    case encryptedMsg of
      Left err -> error $ show err
      Right eMsg -> return eMsg


utilsDecryptECB :: (ByteArray b) => b -> Key AES256 b  -> IO b
utilsDecryptECB eMsg secretKey = do
    let paddedEMsg = pad (ZERO  blockLength) eMsg
    let decryptedMsg = decryptECB secretKey paddedEMsg
    case decryptedMsg of
      Left err -> error $ show err
      Right dMsg -> return dMsg