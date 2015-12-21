{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Char (intToDigit)
import           Graphics.Rendering.Pango.Font
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Gdk.GC
import           Graphics.UI.Gtk.Selectors.FontSelectionDialog
import           Graphics.UI.Gtk.WebKit.WebFrame
import           Graphics.UI.Gtk.WebKit.WebView
import           Graphics.UI.Gtk.Windows.Dialog
import           Numeric (showHex, showIntAtBase)
import qualified Data.Text as T


main = do
  initGUI

  -- Create web vie)uuw
  webView <- webViewNew
  webViewSetEditable webView True
  -- Load HTML
  webViewLoadHtmlString webView
     ("<html><body><h1>Title</h1>Text <b>body</b>...</body></html>"::String)
     ("file:///"::String)

  let ex = executeScript webView
      action' command prefix action text sym = do
        act <- menuItemSym (T.concat [prefix, action, "Action"]) action text sym
        act `on` actionActivated $
          ex (T.concat ["document.execCommand('", command, "', false, false)"])
        return act
      formatAction action text sym = action' action "F" action text sym
      justifyAction name direction text sym =
        action' (T.concat ["justify", direction]) "J" direction text sym

  -- Create the menus
  fileAct <- actionNew' "FileAction" "File" Nothing Nothing
  insertAct <- actionNew' "InsertAction" "Insert" Nothing Nothing
  formatAct <- actionNew' "FormatAction" "Format" Nothing Nothing

  -- Create menu items
  exitAct <- menuItemSym "ExitAction" "Exit" "Exit this application." stockQuit
  exitAct `on` actionActivated $ mainQuit

  showAct <- menuItemSym "ShowHtmlAction" "Show HTML" "Print current HTML on stdout." stockFind
  showAct `on`actionActivated $ do
    html <- getHtml webView
    putStrLn $ case html of 
      Just htmlString -> htmlString
      _ -> "Something went wrong"

  boldAct <- formatAction "Bold" "Apply bold print." stockBold
  italicAct <- formatAction "Italic" "Apply italic print." stockItalic
  underlineAct <- formatAction "Underline" "Underline" stockUnderline

  justifyLeftAct <- justifyAction "Justify Left" "Left" "Align text to the left." stockJustifyLeft
  justifyRightAct <- justifyAction "Justify Right" "Right" "Align text to the right." stockJustifyRight
  justifyCenterAct <- justifyAction "Justify Center" "Center" "Align text to the center." stockJustifyCenter

  fontAct <- menuItemSym "FontAction" "Font" "Set the current font." stockSelectFont
  fontAct `on` actionActivated $ do
    fontDiag <- fontSelectionDialogNew ("Select a font"::T.Text)
    res <- dialogRun fontDiag
    (fontName, fontSize) <- case res of
      ResponseOk -> do
        font <- (fontSelectionDialogGetFontName fontDiag)::IO (Maybe T.Text)
        case font of
          Just str -> (fontDescriptionFromString str) >>= getFont
          _ -> return ("Sans", 10.0)
      _ -> return ("Sans", 10.0)
    ex (T.concat ["document.execCommand('fontname', null, '", fontName, "');"])
    ex (T.concat ["document.execCommand('fontsize', null, '", T.pack (show fontSize), "');"])
    widgetDestroy fontDiag

  colorAct <- menuItemSym "ColorAction" "Color" "Set the current color." stockSelectColor
  colorAct `on` actionActivated $ do
    colorDiag <- colorSelectionDialogNew ("Select a color"::T.Text)
    res <- dialogRun colorDiag
    color <- case res of
      ResponseOk -> colorSelectionDialogGetColor colorDiag
                    >>= colorSelectionGetCurrentColor
                    >>= return . hexColor
      _ -> return "#000000"
    ex (T.concat ["document.execCommand('forecolor', null, '", color, "');"])
    widgetDestroy colorDiag

  -- Put together the menus
  standardGroup <- actionGroupNew ("standard"::T.Text)
  mapM_ (actionGroupAddAction standardGroup) [fileAct, insertAct, formatAct]
  mapM_ (\act -> actionGroupAddActionWithAccel standardGroup act (Nothing::Maybe T.Text))
    [ exitAct, showAct
    , boldAct, italicAct, underlineAct
    , fontAct, colorAct
    , justifyLeftAct, justifyRightAct, justifyCenterAct]

  -- Create UI
  ui <- uiManagerNew
  mid <- uiManagerAddUiFromString ui uiDef
  uiManagerInsertActionGroup ui standardGroup 0

  -- Create window
  win <- windowNew
  on win objectDestroy mainQuit
  on win sizeRequest $ return (Requisition 800 600)
  (Just menuBar) <- uiManagerGetWidget ui ("/ui/menubar"::T.Text)
  (Just toolBar) <- uiManagerGetWidget ui ("/ui/toolbar"::T.Text)

  scrollWin <- scrolledWindowNew Nothing Nothing

  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]
  boxPackStart vBox menuBar PackNatural 0
  boxPackStart vBox toolBar PackNatural 0
  boxPackStart vBox scrollWin PackGrow 0
  scrollWin `containerAdd` webView

  containerAdd win vBox
  widgetShowAll win

  mainGUI


-- | Get the current HTML of an WebView
getHtml :: WebView -> IO (Maybe String)
getHtml webView = do
    executeScript webView "document.title='';"
    executeScript webView "document.title=document.documentElement.innerHTML;"
    frame <- webViewGetMainFrame webView
    webFrameGetTitle' frame


-- | Get font and size from FontDescription
getFont :: FontDescription -> IO (T.Text, Double)
getFont fontDesc = do
  fontFamily <- fontDescriptionGetFamily fontDesc
  fontSize <- fontDescriptionGetSize fontDesc
  return $ case (fontFamily, fontSize) of
    (Just ff, Just fs) -> (ff, fs)
    _ -> ("Sans", 10.0)


-- | Get HTML hex format color
hexColor :: Color -> T.Text
hexColor (Color r g b) =
  let
    scale x = floor ((fromIntegral x) / 256.0)
    channels = [scale r, scale g, scale b]
    show x = T.pack $ showHex x ""
    fmt = (T.justifyRight 2 '0') . show
  in T.concat $ "#" : map fmt channels


uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"Exit\" action=\"ExitAction\"/>\
  \    </menu>\
  \    <menu name=\"Format\" action=\"FormatAction\">\
  \     <menuitem name=\"Bold\" action=\"FBoldAction\" />\
  \     <menuitem name=\"Italic\" action=\"FItalicAction\" />\
  \     <menuitem name=\"Underline\" action=\"FUnderlineAction\" />\
  \     <separator />\
  \     <menuitem name=\"Font\" action=\"FontAction\" />\
  \     <menuitem name=\"Color\" action=\"ColorAction\" />\
  \     <separator />\
  \     <menuitem name=\"Left\" action=\"JLeftAction\" />\
  \     <menuitem name=\"Right\" action=\"JRightAction\" />\
  \     <menuitem name=\"Center\" action=\"JCenterAction\" />\
  \    </menu>\
  \  </menubar>\
  \  <toolbar>\
  \    <placeholder name=\"FormatItems\">\
  \      <toolitem name=\"Font\" action=\"FontAction\"/>\
  \      <toolitem name=\"Color\" action=\"ColorAction\"/>\
  \      <separator/>\
  \      <toolitem name=\"Bold\" action=\"FBoldAction\"/>\
  \      <toolitem name=\"Italic\" action=\"FItalicAction\"/>\
  \      <toolitem name=\"Underline\" action=\"FUnderlineAction\"/>\
  \    </placeholder>\
  \    <placeholder name=\"JustifyItems\">\
  \      <separator/>\
  \      <toolitem name=\"Left\" action=\"JLeftAction\"/>\
  \      <toolitem name=\"Right\" action=\"JRightAction\"/>\
  \      <toolitem name=\"Center\" action=\"JCenterAction\"/>\
  \    </placeholder>\
  \    <placeholder name=\"OtherItems\">\
  \      <separator/>\
  \      <toolitem name=\"Show HTML\" action=\"ShowHtmlAction\"/>\
  \    </placeholder>\
  \  </toolbar>\
  \</ui>" :: T.Text


-- | Necessary to clarify types
webFrameGetTitle' :: WebFrame -> IO (Maybe String)
webFrameGetTitle' = webFrameGetTitle

executeScript :: WebView -> T.Text -> IO ()
executeScript = webViewExecuteScript

actionNew' :: T.Text -> T.Text -> (Maybe T.Text) -> (Maybe StockId) -> IO Action
actionNew' = actionNew

menuItemSym :: T.Text -> T.Text -> T.Text -> StockId -> IO Action
menuItemSym action name text sym =
  actionNew action name (Just text) (Just sym)
