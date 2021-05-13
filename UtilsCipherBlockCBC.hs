{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module UtilsCipherBlockCBC(
    utilsDecryptCBC,
    utilsEncryptCBC
    )where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import           Crypto.Error (CryptoFailable(..), CryptoError(..))

import Crypto.Data.Padding (pad, unpad, Format(ZERO))
import qualified Crypto.Random.Types as CRT

import           Data.ByteArray (ByteArray)
import           Data.ByteString (ByteString)

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


utilsEncryptCBC :: ByteString -> Key AES256 ByteString -> Maybe (IV AES256) -> IO ByteString
utilsEncryptCBC msg secretKey mInitIV = do
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let paddedMsg = pad (ZERO  blockLength) msg
      let encryptedMsg = encryptCBC secretKey initIV paddedMsg
      case encryptedMsg of
        Left err -> error $ show err
        Right eMsg -> do
          return eMsg


utilsDecryptCBC :: ByteString -> Key AES256 ByteString -> Maybe (IV AES256) -> IO ByteString
utilsDecryptCBC eMsg secretKey mInitIV = do
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let paddedEMsg = pad (ZERO  blockLength) eMsg
      let decryptedMsg = decryptCBC secretKey initIV paddedEMsg
      case decryptedMsg of
        Left err -> error $ show err
        Right dMsg -> do
          return dMsg