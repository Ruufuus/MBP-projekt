{-# LANGUAGE MonoLocalBinds, TypeApplications #-}

import Data.Text (Text)
import Data.Text.Encoding 
import qualified Data.Text as Text
import Data.Fixed (Centi)
import Control.Concurrent (threadDelay, forkIO)
import System.Random (randomRIO)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 (unpack)
import Text.Read (readMaybe)
import Text.Printf (printf)
import Control.Monad (unless)
import UtilsAES
import UtilsCipherBlockECB
import UtilsCipherBlockCTR
import UtilsCipherBlockCBC
import           Crypto.Cipher.AES (AES256)

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib

main :: IO ()
main = do
  Just app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (appActivate app)
  _ <- Gio.applicationRun app Nothing
  return ()

appId :: Text
appId = Text.pack "io.serokell.gui-haskell-app"

appActivate :: Gtk.Application -> IO ()
appActivate app = do
  secretKey <- genSecretKey (undefined :: AES256) 32
  mInitIV <- genRandomIV (undefined :: AES256)
  window <- Gtk.applicationWindowNew app
  Gtk.setWindowTitle window (Text.pack "Szyfrowanie")
  Gtk.setWindowResizable window False
  Gtk.setWindowDefaultWidth window 500
  vbox <- Gtk.boxNew Gtk.OrientationVertical 10
  Gtk.setWidgetMargin vbox 10
  Gtk.containerAdd window vbox
  Gtk.widgetShow vbox
  message <- addEntry (Text.pack "Wiadomosc") vbox
  eMessage <- addEntry (Text.pack "Szyfrogram") vbox
  dMessage <- addEntry (Text.pack "Odszyfrowana wiadomosc") vbox
  button <- Gtk.buttonNew
  Gtk.setButtonLabel button (Text.pack "Zaszyfruj wiadomosc")
  Gtk.setWidgetHalign button Gtk.AlignCenter
  Gtk.containerAdd vbox button
  _ <- Gtk.onButtonClicked button $
    do 
      msg <- Gtk.entryGetText message
      let msgBytes = pack (Text.unpack (msg))
      eMsg <- utilsEncryptCBC msgBytes secretKey mInitIV
      dMsg <- utilsDecryptCBC eMsg secretKey mInitIV
      Gtk.entrySetText eMessage (Text.pack (unpack eMsg))
      Gtk.entrySetText dMessage (Text.pack (unpack dMsg))
      printf "Bazowa wiadomosc: %s\n" (unpack msgBytes)
      printf "Zaszyfrowana wiadomosc: %s\n" (unpack eMsg)
      printf "Odszyfrowana wiadomosc: %s\n" (unpack dMsg)

  Gtk.widgetShow button
  Gtk.widgetShow window


addEntry :: Gtk.IsContainer a => Text -> a -> IO Gtk.Entry
addEntry labelStr container = do
  hbox <- Gtk.boxNew Gtk.OrientationHorizontal 5
  entry <- Gtk.entryNew
  Gtk.setWidgetExpand entry True
  Gtk.setEntryXalign entry 0
  label <- Gtk.labelNew (Just labelStr)
  Gtk.containerAdd hbox label
  Gtk.containerAdd hbox entry
  Gtk.containerAdd container hbox
  Gtk.widgetShow entry
  Gtk.widgetShow label
  Gtk.widgetShow hbox
  return entry

