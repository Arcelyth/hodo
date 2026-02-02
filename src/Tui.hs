{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Tui where

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import Lens.Micro (Lens', lens, (^.), (&), (%~), (.~))
import Lens.Micro.Mtl (use, zoom, (.=), (%=))
import Control.Monad.State (get, put)

import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Types (Widget)
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hBox, str, vBox, withAttr, withBorderStyle
  , (<+>), padAll, padTop, Padding(..), forceAttr,  updateAttrMap, padRight 
  )
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as F
import qualified Data.Vector as Vec
import qualified Data.List as DL
import Lens.Micro.TH (makeLenses)
import Control.Monad.IO.Class (liftIO)
import qualified Brick.Widgets.Edit as E
import Data.Text (Text)
import qualified Data.Text as DT

data Name = PendingList | WipList | DoneList | AddEditor | ModifyEditor
  deriving (Show, Eq, Ord)

data Mode = Normal | Change | Add | Modify

data AppState = AppState
  { _pList :: L.List Name String
  , _wList     :: L.List Name String
  , _dList    :: L.List Name String
  , _focusRing   :: F.FocusRing Name
  , _mode        :: Mode
  , _editor      :: E.Editor Text Name
  }
makeLenses ''AppState

nameToLens :: Name -> Lens' AppState (L.List Name String)
nameToLens PendingList = pList 
nameToLens WipList     = wList 
nameToLens DoneList    = dList 

attrPending, attrWip, attrDone, attrChange :: A.AttrName
attrPending = A.attrName "pending"
attrWip     = A.attrName "wip"
attrDone    = A.attrName "done"
attrChange  = A.attrName "change"
attrEditor  = A.attrName "editor"
attrHodo = A.attrName "hodo"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (attrPending, fg V.brightBlue)
  , (attrWip,     fg V.brightYellow)
  , (attrDone,    fg V.brightGreen)
  , (attrChange,  V.black `on` V.cyan)
  , (attrEditor,  fg V.brightBlue)
  , (attrHodo,  fg V.brightMagenta)
  ]

drawUI :: AppState -> [Widget Name]
drawUI s = 
  case s^.mode of
    Add  -> [drawAddUI s]
    Modify -> [drawModifyUI s]
    _    -> [ui]
  where
    focus = F.focusGetCurrent (s^.focusRing)

    drawList name title colorAttr l =
      let isFocused = focus == Just name
          borderStyle = if isFocused then BS.unicodeBold else BS.unicode
          mappings = A.applyAttrMappings [(B.borderAttr, A.attrMapLookup colorAttr theMap)]
      in 
         updateAttrMap mappings $
         withBorderStyle borderStyle $
         withAttr colorAttr $
         B.borderWithLabel (str title) $
         L.renderList (drawItem s isFocused) isFocused l

    ui = 
      C.center $
      updateAttrMap (A.applyAttrMappings [(B.borderAttr, A.attrMapLookup attrHodo theMap)]) $
      withAttr attrHodo $
      B.borderWithLabel (str " HODO ") $
      padAll 1 $
      hBox
        [ drawList PendingList " Pending "  attrPending (s^.pList)
        , drawList WipList     " W.I.P "      attrWip     (s^.wList)
        , drawList DoneList    " Done "     attrDone    (s^.dList)
        ]

drawAddUI :: AppState -> Widget Name
drawAddUI s =
  forceAttr attrEditor $
  C.centerLayer $
  B.borderWithLabel (str " Add Item ") $
  padAll 1 $
  vBox
    [ str "Enter new todo:"
    , padTop (Pad 1) $
      E.renderEditor (str . DT.unpack . DT.unlines) True (s^.editor)
    , padTop (Pad 1) $
      str "Enter = confirm | Esc = cancel"
    ]

drawModifyUI :: AppState -> Widget Name
drawModifyUI s =
  forceAttr attrEditor $
  C.centerLayer $
  B.borderWithLabel (str " Modify Item ") $
  padAll 1 $
  vBox
    [ str "Edit item:" 
    , padTop (Pad 1) $
      E.renderEditor (str . DT.unpack . DT.unlines) True (s^.editor)
    , padTop (Pad 1) $
      str "Enter = save | Esc = cancel"
    ]

drawItem :: AppState -> Bool -> Bool -> String -> Widget Name
drawItem s isFocused selected txt =
    let 
        prefix = if selected
                   then (if isFocused then "> " else "  ") 
                   else "  "                               
        itemWidget = padRight Max $ str (prefix <> txt)

    in if selected && isFocused
       then case s^.mode of
              Change -> withAttr attrChange itemWidget
              Normal -> itemWidget
       else 
           itemWidget

moveItemHoriz :: Name -> Name -> AppState -> AppState
moveItemHoriz from to s =
  case L.listSelectedElement (s^.nameToLens from) of
    Nothing -> s
    Just (idx, val) ->
      s & nameToLens from %~ L.listRemove idx
        & nameToLens to   %~ L.listInsert 0 val
        & nameToLens to   %~ L.listMoveTo 0
        & focusRing      %~ F.focusSetCurrent to

moveItemVert :: Int -> L.List Name String -> L.List Name String
moveItemVert delta l =
  case L.listSelectedElement l of
    Nothing -> l
    Just (idx, val) ->
      let newIdx = idx + delta
          len = Vec.length (L.listElements l)
      in if newIdx < 0 || newIdx >= len
         then l
         else l
              & L.listRemove idx
              & L.listInsert newIdx val
              & L.listMoveTo newIdx

removeSelected :: Lens' AppState (L.List Name String) -> AppState -> AppState
removeSelected listLens s =
  case L.listSelected (s^.listLens) of
    Nothing  -> s
    Just idx -> s & listLens %~ L.listRemove idx

syncFile :: AppState -> IO()
syncFile s = do
  let render pre l =
        [ pre <> "|" <> x
        | x <- Vec.toList (L.listElements l)
        ]
      content = unlines $
           render "P" (s^.pList)
        ++ render "W" (s^.wList)
        ++ render "D" (s^.dList)
  home <- getHomeDirectory
  let path = home </> ".hodo"
  writeFile path content

appEvent :: T.BrickEvent Name e -> T.EventM Name AppState ()
appEvent evv@(T.VtyEvent e) = do
  s <- get
  case s^.mode of
    Normal -> case e of
      V.EvKey V.KEsc [] -> M.halt
      V.EvKey V.KLeft  [] -> focusRing %= F.focusPrev
      V.EvKey V.KRight [] -> focusRing %= F.focusNext
      V.EvKey (V.KChar 'c') [] -> mode .= Change
      V.EvKey (V.KChar 'a') [] -> do
        mode .= Add
        editor .= E.editor AddEditor (Just 1) ""
      V.EvKey (V.KChar 'm') [] -> do
        r <- use focusRing
        s' <- get
        let maybeSelected = case F.focusGetCurrent r of
              Just PendingList -> L.listSelectedElement (s'^.pList)
              Just WipList     -> L.listSelectedElement (s'^.wList)
              Just DoneList    -> L.listSelectedElement (s'^.dList)
              Nothing          -> Nothing
        case maybeSelected of
          Nothing -> return () 
          Just (_, val) -> do
            mode .= Modify
            editor .= E.editor ModifyEditor (Just 1) (DT.pack val)
      V.EvKey (V.KChar 'r') [] -> do
        r <- use focusRing
        s' <- get
        let s'' = case F.focusGetCurrent r of
              Just PendingList -> removeSelected pList s'
              Just WipList     -> removeSelected wList s'
              Just DoneList    -> removeSelected dList s'
              Nothing          -> s'
        put s''
        liftIO (syncFile s'')

      ev -> do
        r <- use focusRing
        case F.focusGetCurrent r of
          Just PendingList -> zoom pList $ L.handleListEvent ev
          Just WipList     -> zoom wList     $ L.handleListEvent ev
          Just DoneList    -> zoom dList    $ L.handleListEvent ev
          Nothing -> return ()

    Change -> case e of
      V.EvKey V.KEsc [] -> mode .= Normal

      V.EvKey V.KLeft [] -> do
        r <- use focusRing
        s' <- get
        case F.focusGetCurrent r of
          Just WipList  -> do
            let s'' = moveItemHoriz WipList PendingList s'
            put s''
            liftIO (syncFile s'')
          Just DoneList -> do 
            let s'' = moveItemHoriz DoneList WipList s'
            put s''
            liftIO (syncFile s'')
          _ -> return ()

      V.EvKey V.KRight [] -> do
        r <- use focusRing
        s' <- get
        case F.focusGetCurrent r of
          Just PendingList -> do
            let s'' = moveItemHoriz PendingList WipList s'
            put s''
            liftIO (syncFile s'')
          Just WipList     -> do
            let s'' = moveItemHoriz WipList DoneList s'
            put s''
            liftIO (syncFile s'')
          _ -> return ()

      V.EvKey V.KUp [] -> do
        r <- use focusRing
        s' <- get
        case F.focusGetCurrent r of
          Just n -> do
            let s'' = s' & nameToLens n %~ moveItemVert (-1)
            put s''
            liftIO (syncFile s'')
          Nothing -> return ()
     
      V.EvKey V.KDown [] -> do
        r <- use focusRing
        s' <- get
        case F.focusGetCurrent r of
          Just n -> do
            let s'' = s' & nameToLens n %~ moveItemVert 1
            put s''
            liftIO (syncFile s'')
          Nothing -> return ()

      _ -> return ()
    Add -> case e of 
      V.EvKey V.KEsc [] -> do
        mode .= Normal
      V.EvKey V.KEnter [] -> do
        s <- get
        let txt = 
             DT.unpack $ 
             DT.intercalate "\n" $
             E.getEditContents (s^.editor)

        if null txt
          then mode .= Normal
          else do 
            let s' = 
                 s & pList %~ L.listInsert
                     (Vec.length (L.listElements (s^.pList)))
                     txt
                   & mode .~ Normal
            put s'
            liftIO (syncFile s')
      _ -> zoom editor (E.handleEditorEvent evv)
    Modify -> case e of 
      V.EvKey V.KEsc [] -> mode .= Normal
      V.EvKey V.KEnter [] -> do
        s <- get
        let txt = DT.unpack $ DT.intercalate "\n" $ E.getEditContents (s^.editor)
        r <- use focusRing
        
        if null txt 
          then mode .= Normal
          else do
            let s' = case F.focusGetCurrent r of
                       Just n  -> s & nameToLens n %~ L.listModify (const txt)
                       Nothing -> s
            let finalState = s' & mode .~ Normal
            put finalState
            liftIO (syncFile finalState)
            
      _ -> zoom editor (E.handleEditorEvent evv)

appEvent _ = return ()

app :: M.App AppState e Name
app = M.App
  { M.appDraw         = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent  = appEvent
  , M.appStartEvent   = return ()
  , M.appAttrMap      = const theMap
  }

tui :: IO ()
tui = do
  home <- getHomeDirectory
  let path = home </> ".hodo"
  exists <- doesFileExist path
  if not exists then writeFile path "" else return ()
  content <- readFile path
  let ls = lines content
      findPrefix p = Vec.fromList [ drop 2 l | l <- ls, p `DL.isPrefixOf` l ]
      initState = AppState
        { _pList = L.list PendingList (findPrefix "P|") 1
        , _wList     = L.list WipList     (findPrefix "W|") 1
        , _dList    = L.list DoneList    (findPrefix "D|") 1
        , _focusRing   = F.focusRing [PendingList, WipList, DoneList]
        , _mode        = Normal
        , _editor = E.editor AddEditor (Just 1) ""
        }
  void $ M.defaultMain app initState
