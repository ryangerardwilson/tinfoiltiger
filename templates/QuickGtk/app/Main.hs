module Main (main) where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle window (T.pack "Haskell GTK App (Stack Edition)")
  _ <- Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.main
