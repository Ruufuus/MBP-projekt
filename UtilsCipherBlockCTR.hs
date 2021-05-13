{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module UtilsCipherBlockCTR(
    utilsDecryptCTR,
    utilsEncryptCTR
    )where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import           Crypto.Error (CryptoFailable(..), CryptoError(..))

import Crypto.Data.Padding (pad, unpad, Format(ZERO))
import qualified Crypto.Random.Types as CRT

import           Data.ByteArray (ByteArray)
import           Data.ByteString (ByteString)

import UtilsAES


encryptCTR :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encryptCTR secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ctrCombine c initIV msg

decryptCTR :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decryptCTR = encryptCTR



utilsEncryptCTR :: ByteString -> Key AES256 ByteString -> Maybe (IV AES256) -> IO ByteString
utilsEncryptCTR msg secretKey mInitIV = do
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let encryptedMsg = encryptCTR secretKey initIV msg
      case encryptedMsg of
        Left err -> error $ show err
        Right eMsg -> do
          return eMsg


utilsDecryptCTR :: ByteString -> Key AES256 ByteString -> Maybe (IV AES256) -> IO ByteString
utilsDecryptCTR eMsg secretKey mInitIV = do
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      let decryptedMsg = decryptCTR secretKey initIV eMsg
      case decryptedMsg of
        Left err -> error $ show err
        Right dMsg -> do
          return dMsg