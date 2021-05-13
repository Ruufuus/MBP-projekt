{-# LANGUAGE MonoLocalBinds, TypeApplications #-}

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 (unpack)
import Text.Printf (printf)
import UtilsAES
import UtilsCipherBlockECB
import UtilsCipherBlockCTR
import UtilsCipherBlockCBC
import           Crypto.Cipher.AES (AES256)

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio

main :: IO ()
main = do
  Just app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (appActivate app)
  _ <- Gio.applicationRun app Nothing
  return ()

appId :: Text
appId = Text.pack "io.szyfry-blokowe-app"

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

  buttonCTR <- Gtk.buttonNew
  buttonECB <- Gtk.buttonNew
  buttonCBC <- Gtk.buttonNew

  Gtk.setButtonLabel buttonCTR (Text.pack "[CTR] Zaszyfruj wiadomosc")
  Gtk.setButtonLabel buttonECB (Text.pack "[ECB] Zaszyfruj wiadomosc")
  Gtk.setButtonLabel buttonCBC (Text.pack "[CBC] Zaszyfruj wiadomosc")

  Gtk.setWidgetHalign buttonCTR Gtk.AlignCenter
  Gtk.setWidgetHalign buttonECB Gtk.AlignCenter
  Gtk.setWidgetHalign buttonCBC Gtk.AlignCenter

  Gtk.containerAdd vbox buttonCBC
  Gtk.containerAdd vbox buttonECB
  Gtk.containerAdd vbox buttonCTR

  _ <- Gtk.onButtonClicked buttonCBC $
    do 
      msg <- Gtk.entryGetText message
      let msgBytes = pack (Text.unpack (msg))
      eMsg <- utilsEncryptCBC msgBytes secretKey mInitIV
      dMsg <- utilsDecryptCBC eMsg secretKey mInitIV
      Gtk.entrySetText eMessage (Text.pack (unpack eMsg))
      Gtk.entrySetText dMessage (Text.pack (unpack dMsg))
      printf "Bazowa wiadomosc: %s\n" (unpack msgBytes)
      printf "[CBC]\tZaszyfrowana wiadomosc: %s\n" (unpack eMsg)
      printf "[CBC]\tOdszyfrowana wiadomosc: %s\n" (unpack dMsg)

  _ <- Gtk.onButtonClicked buttonECB $
    do 
      msg <- Gtk.entryGetText message
      let msgBytes = pack (Text.unpack (msg))
      eMsg <- utilsEncryptECB msgBytes secretKey 
      dMsg <- utilsDecryptECB eMsg secretKey 
      Gtk.entrySetText eMessage (Text.pack (unpack eMsg))
      Gtk.entrySetText dMessage (Text.pack (unpack dMsg))
      printf "Bazowa wiadomosc: %s\n" (unpack msgBytes)
      printf "[ECB]\tZaszyfrowana wiadomosc: %s\n" (unpack eMsg)
      printf "[ECB]\tOdszyfrowana wiadomosc: %s\n" (unpack dMsg) 

  _ <- Gtk.onButtonClicked buttonCTR $
    do 
      msg <- Gtk.entryGetText message
      let msgBytes = pack (Text.unpack (msg))
      eMsg <- utilsEncryptCTR msgBytes secretKey mInitIV
      dMsg <- utilsDecryptCTR eMsg secretKey mInitIV
      Gtk.entrySetText eMessage (Text.pack (unpack eMsg))
      Gtk.entrySetText dMessage (Text.pack (unpack dMsg))
      printf "Bazowa wiadomosc: %s\n" (unpack msgBytes)
      printf "[CTR]\tZaszyfrowana wiadomosc: %s\n" (unpack eMsg)
      printf "[CTR]\tOdszyfrowana wiadomosc: %s\n" (unpack dMsg)

  Gtk.widgetShow buttonECB
  Gtk.widgetShow buttonCTR
  Gtk.widgetShow buttonCBC
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

