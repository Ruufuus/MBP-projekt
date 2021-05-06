module Lib
    ( someFunc
    ) where


import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV)
import Crypto.Error (CryptoFailable(..))        

someFunc :: IO ()
someFunc = putStrLn "someFunc2"
