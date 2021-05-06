module Main where
import Control.Monad (void)
import Data.Maybe
import Data.ByteString.Char8 (pack)
import Text.Printf

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import UtilsCipherChain

main :: IO ()
main = do
    let msg = pack "moje wiadomosc"
    exampleAES256 msg
    startGUI defaultConfig setup
    

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Program do szyfrowania wiadomosci"


    tekst <- UI.input

    getBody window #+ [
                column [
                    grid [[string "Wpisz tekst do zaszyfrowania:", element tekst]]
                ]]

    element tekst