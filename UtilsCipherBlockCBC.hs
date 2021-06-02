{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module UtilsCipherBlockCBC(
    utilsDecryptCBC,
    utilsEncryptCBC
    )where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), IV)
import           Crypto.Error ( CryptoError(..))

import Crypto.Data.Padding (pad, Format(ZERO))

import           Data.ByteArray (ByteArray)

import UtilsAES


encryptCBC :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encryptCBC secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ cbcEncrypt c initIV msg

decryptCBC :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decryptCBC secretKey initIV msg = 
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ cbcDecrypt c initIV msg


utilsEncryptCBC :: (ByteArray b) => b -> Key AES256 b -> Maybe (IV AES256) -> IO b
utilsEncryptCBC msg secretKey mInitIV = do
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let paddedMsg = pad (ZERO  blockLength) msg
      let encryptedMsg = encryptCBC secretKey initIV paddedMsg
      case encryptedMsg of
        Left err -> error $ show err
        Right eMsg -> return eMsg


utilsDecryptCBC :: (ByteArray b) => b -> Key AES256 b -> Maybe (IV AES256) -> IO b
utilsDecryptCBC eMsg secretKey mInitIV = do
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let paddedEMsg = pad (ZERO  blockLength) eMsg
      let decryptedMsg = decryptCBC secretKey initIV paddedEMsg
      case decryptedMsg of
        Left err -> error $ show err
        Right dMsg -> return dMsg