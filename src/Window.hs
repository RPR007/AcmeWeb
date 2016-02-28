{-# LANGUAGE PatternSynonyms, ForeignFunctionInterface,JavaScriptFFI,CPP,RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Window where

import Reflex.Dom
import Reflex.Contrib.Window
import Reflex.Dom.WebSocket
import GHCJS.DOM.Document hiding (dragStart,drop,click)
import GHCJS.DOM.Element hiding (drop)
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.EventM
import GHCJS.Marshal
import GHCJS.DOM.Node

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Default
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Char8 (unpack,pack)
import Data.ByteString.Base64
import Data.List.Utils
    
import Nav
import Ace
import Term
import Webdav
import Utils
import Command hiding (getMouseEventCoords)
    
import Control.Lens
import Data.Dependent.Sum (DSum (..))

import Data.IORef
import Foreign.Ptr
import Control.Concurrent
import Control.Monad.Fix


-- Model
data WindowType = File | Term | Frame deriving (Eq,Show)
                
data Window
   = Window 
        { _window_id :: Int
        , _window_lock :: Maybe (Int,Int)
        , _window_name :: String
        , _window_type :: WindowType
        , _window_height :: Double
        }  deriving (Eq,Show)

data WindowDrop
    = WindowDrop 
        { _window_drag :: Bool
        , _window_drop :: Bool
        } deriving (Eq)
                                  

data Drop
    = AcmeDrop Int | ColumDrop Int deriving (Eq,Show)
    
data Windows
    =  Windows 
        { _client_width :: Int
        , _client_height :: Int
        , _windows :: [Window] 
        }
      
data WindowsConfig t m b
    = WindowsConfig 
        { _windowsConfig_id :: m (MVar Int)
        , _windowsConfig_columId :: Dynamic t Int
        , _windowsConfig_newWindow :: Event t ()
        , _windowsConfig_newDir :: Event t ()
        , _windowsConfig_newTerm :: Event t ()
        , _windowsConfig_drop :: Event t Int
        , _windowsConfig_columDrop :: Event t Int
        , _windowsConfig_list :: Dynamic t [(Int,Window)]
        , _windowsConfig_add :: Event t (Int,Window)
        , _windowsConfig_windows :: (MVar Int) -> m Windows
        , _windowsConfig_stateRef :: Maybe (IORef [((Int, (Event t (Windows -> Windows), Event t (Int,Window))), WidgetHost m (),Event t (WidgetHost m ()))])
        }
        
      
instance (Reflex t,MonadWidget t m)  => Default (WindowsConfig t m b) where
  def = WindowsConfig 
            { _windowsConfig_id = liftIO $ newMVar (0 :: Int)
            , _windowsConfig_columId = constDyn 0
            , _windowsConfig_newWindow = never
            , _windowsConfig_newDir = never
            , _windowsConfig_newTerm = never
            , _windowsConfig_drop = never
            , _windowsConfig_columDrop = never
            , _windowsConfig_list = constDyn []
            , _windowsConfig_add = never
            , _windowsConfig_windows = defWindows
            , _windowsConfig_stateRef = Nothing
            }

defWindows :: MonadWidget t m => (MVar Int) -> m Windows
defWindows refId = do
   windowDimensions <- windowDimensions
   (width,height) <- sample $ current windowDimensions

   id1 <- liftIO $ readMVar refId
   liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                                
   return $ Windows width (height-48)
               [ Window id1 Nothing "/" Frame ((fromIntegral (height-48)))
               ]
 -- Lens

liftM concat $ mapM makeLenses
  [ ''Window
  , ''WindowDrop
  , ''Windows
  , ''WindowsConfig
  ]
  
-- View
wWindows :: MonadWidget t m => WindowsConfig t m b -> m (Event t (Int,Window), Dynamic t [(Int,Window)])
wWindows (WindowsConfig refIdM columId eNewWindow eNewDir eNewTerm eDrop eColumsDrop dWindowsList eAdd vWindowsM stateRef) = do
  rec refId <- refIdM
      vWindows <- vWindowsM refId
      let eAdd' = fmap (\(_,(_,vWindow)) -> vWindow) $ ffilter (\(columId,(columId',_)) -> columId == columId') $ attachDyn columId eAdd
      fupdate <- mapDyn windowsToList =<< update refId vWindows eNewWindow eNewDir eNewTerm eWindows eAdd'
      (eWindows,eWindowAdd) <- do
                rec let win = window refId columId eNewWindow (leftmost 
                                                                [ffor eColumsDrop ColumDrop])
                    let (Just stateRef') = stateRef
                    windowsEv <- wSimpleList stateRef' dWindowsList fupdate win
                    dynEvWindows <- mapDyn (\evList -> leftmost $ map fst evList) windowsEv
                    let _windowsEv = switchPromptlyDyn dynEvWindows
                    
                    dynEvAdd <- mapDyn (\evList -> leftmost $ map snd evList) windowsEv
                    let eWindowAdd = switchPromptlyDyn dynEvAdd
               
                return (_windowsEv,eWindowAdd)
  return (eWindowAdd,fupdate)
 
windowsToList :: [Window] -> [(Int,Window)]
windowsToList vWindows = foldl helper [] vWindows
    where
      helper acc vWindow = acc ++ [(_window_id vWindow, vWindow)]
                           
window :: MonadWidget t m
          => MVar Int
          -> Dynamic t Int
          -> Event t ()
          -> Event t Drop
          -> Dynamic t Window
          -> m (Event t (Windows -> Windows), Event t (Int,Window))
window refId dColumId eNewWindow eDrop dWindow = do
      doc <- askDocument
      postBuild <- getPostBuild
      
      dId <- mapDyn _window_id dWindow
      dName <- mapDyn _window_name dWindow
      dType <- mapDyn _window_type dWindow
      dHeight <- mapDyn _window_height dWindow
      
      let eDropColum = ffilter (\x -> case x of
                                        ColumDrop _ -> True
                                        x -> False ) $ eDrop
                
      dCombine <- combineDyn (\a (b,c,d) -> (b,c,d,a)) dHeight
                  =<< combineDyn (\a (b,c) -> (b,c,a))  dType
                  =<< combineDyn (\a b -> (a,b)) dId dName

      
      winDynAttr <- mapDyn (\(_,_,_type,height) -> "class" =: "window"
                                    <> "style" =: ("height : " ++ show height ++ "px;")) dCombine
      elDynAttr "div" winDynAttr $ do  
        (eClick,eDrag,eExec) <- nav dName "Del Get Put"
        dAttrs <- forDyn dHeight $ \height ->
                       "draggable" =: "true"
                    <> "style" =: (
                        (if height > 25
                         then "display:inline-block;"
                         else "display:none;") ++
                        "vertical-align : top;" ++
                        "cursor : pointer;" ++
                        "border : none;" ++
                        "background-color : #000000;" ++
                        "width:5px;height : " ++ show (height-25) ++ "px;")
        (elLeft,_) <- elDynAttr' "div" dAttrs $ blank
        eUp <- wrapDomEvent (_el_element elLeft) (`on` click) $ getMouseEventCoords
        eLeftDragstart <- wrapDomEvent (_el_element elLeft) (`on` dragStart) $ 
                        event >>= (liftIO . dataTransfer)
        eLeftDragstart' <- performEvent $ fmap return eLeftDragstart
         
        let ePut = ffilter (=="Put")  eExec
        let eGet = ffilter (=="Get") eExec
        (fileEl,evFile) <- file postBuild eNewWindow eDrop ePut eGet dCombine dHeight dName dType dId
        (frameEl,evFrame) <- frame refId postBuild eGet dCombine dName dType
        (termEl,evTerm) <- terminal postBuild dCombine dType eExec
                
        isDrop <- isWindowDrop (leftmost [eDrag,eLeftDragstart]) eDropColum
        dDrop <- holdDyn (ColumDrop (-1)) eDropColum 
        dDropInt <- forDyn dDrop $ \(ColumDrop x) -> x
        let winDrop = attachDyn dDropInt $ tag (current dWindow) $ ffilter _window_drop $ updated isDrop
        
        let eWindowDelete = tag (current dId) $ ffilter (=="Del") eExec
        performEvent_ $ ffor eWindowDelete $ \id -> do
            (Just el) <- getElementById doc (show id)
            mp <- getParentNode el
            forM_ mp $ \p -> removeChild p (Just el)
            
        let eNewTerm = ffilter (=="Term")  eExec
        performENewTerm <- performEvent $ ffor eNewTerm $ \_ -> do
                                 id <- liftIO $ readMVar refId
                                 liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                                 return id 
                                                     
        let eWindowResize = attachWith (\a b -> (a,b)) (current dId) (leftmost [eClick,eUp])
  
        
        let eWindow = leftmost 
                [
               ffor eWindowDelete $ \vId model -> model & windows .~ (window_delete vId (_windows model))
              ,ffor (tag (current dId) winDrop) $ \vId model -> model & windows .~ (window_delete vId (_windows model))
              ,ffor eWindowResize $ \(vId,coords) model -> model & windows .~ (window_resize vId coords (_windows model))
              ,ffor (tag (current dId) winDrop) $ \vId model -> model & windows .~ (window_delete vId (_windows model))
              ,fmap (window_new Term "Term") performENewTerm
              ,evFile
              ,evFrame
                ]
                
        return (eWindow,winDrop)

isWindowDrop :: MonadWidget t m => Event t () -> Event t Drop -> m (Dynamic t WindowDrop)
isWindowDrop eDrag eDrop = do
    dyn <- foldDyn ($) (WindowDrop False False) $ mergeWith (.)
             [
               ffor eDrag $ \_ model ->
                   WindowDrop True False
              ,ffor eDrop $ \_ model ->
                  let eDrag = _window_drag model
                  in if eDrag
                     then WindowDrop False True
                     else model
             ]
    return $ nubDyn dyn
    
file :: MonadWidget t m
        => Event t ()
        -> Event t ()
        -> Event t Drop
        -> Event t String
        -> Event t String
        -> Dynamic t (Int,String,WindowType,Double)
        -> Dynamic t Double
        -> Dynamic t String
        -> Dynamic t WindowType
        -> Dynamic t Int
        -> m (El t,Event t (Windows -> Windows))
file postBuild eNewWindow eDrop ePut eGet dCombine dHeight dName dType dId = do
    rec dynFileAttrs <- forDyn dCombine $ \(id,name,windowType,height) ->
                    "id" =: ("f" ++ (show id))
                <>  "class" =: "ace_editor ace-merbivore-soft ace_dark"
                <>  "style" =: ("position:relative;" ++
                                "overflow-y : hidden;" ++ 
                                "font-size:18px;" ++
                                "display : " ++ 
                                    (if height > 25 && windowType == File
                                    then "inline-block;"
                                    else "none;") ++ 
                                "height:" ++ (show $ height-25) ++ "px;" ++
                                "width:calc(100% - 5px);") 
    
        (fileEl,_) <- elDynAttr' "div" dynFileAttrs $ blank
    
        let isFile (_,_,windowType,_) = windowType == File
        let initEv = ffilter isFile $ leftmost [tag (current dCombine) postBuild]

        eEditor <- performEvent $ ffor (tagDyn dId initEv) $ \id -> do
                    jsId <- liftIO $ toJSVal ("f" ++ show id)
                    liftIO $ ace jsId
                           
        dEditor <- holdDyn nullPtr eEditor

        
        -- Get File Text
        let readFileEv =  ffilter (\(_,_dName,_windowType,_) -> _windowType == File && _dName /= "untitle") $ tag (current dCombine) (leftmost [postBuild, tagDyn (constDyn ()) eGet])
        txt <- Webdav.read (leftmost [tag (constant ()) readFileEv]) dName
        let attachTxt = attachDyn (nubDyn dName) (attachDyn dEditor (updated txt))
                      
        performEvent_ $ ffor attachTxt $ \(name,(ed,txt)) -> do
                            jsText <- liftIO $ toJSVal txt
                            jsMode <- liftIO $ toJSVal $ last $ split "." name  
                            liftIO $ aceMode ed jsMode
                            liftIO $ aceSetValue ed jsText
                            liftIO $ aceGotoLine ed 1 0 False
                            liftIO $ acePageUp ed
                       
        -- put Text
        let attachPut = tagDyn dEditor ePut
        eValPut <- performEvent $ ffor attachPut $ \ed ->
                        if ed /= nullPtr
                          then do
                            jsVal <- liftIO $ aceGetValue ed
                            (Just val) <- liftIO $ (fromJSVal jsVal)
                            return val
                          else
                            return ""
                            
        putRep <- write eValPut dName
              
        -- Event Resize Editor
        let eTagNewWindow = tagDyn dEditor $ ffilter isFile $ tagDyn dCombine eNewWindow
        let eTagColumResize =  tagDyn dEditor $ ffilter isFile $ tag (current dCombine) eDrop
        let eTagEHeight = tagDyn dEditor $ ffilter isFile $ tag (current dCombine) (updated  dHeight) 
                    
        performEvent_ $ ffor (leftmost [eTagColumResize,eTagNewWindow,eTagEHeight]) $ \ed ->
             --   if ed /= nullPtr 
         --         then  
                    liftIO $ void $ forkIO $ do
                     threadDelay 100000 
                     liftIO $ aceResize ed 
           --       else return ()

    let ev = leftmost
           [
                
           ]
           
    return (fileEl,ev)

terminal :: MonadWidget t m
        => Event t ()
        -> Dynamic t (Int,String,WindowType,Double)
        -> Dynamic t WindowType
        -> Event t String
        -> m (El t,Event t (Windows -> Windows)) 
terminal postBuild dCombine dType eExec = do
    rec dynTermAttrs <- forDyn dCombine $ \(id,name,windowType,height) ->
                    "id" =: ("t" ++ (show id))
                <>  "style" =: ("overflow : hidden;" ++ 
                                "font-size:18px;" ++
                                "border : none;" ++
                                "display : " ++ 
                                    (if height > 25 && windowType == Term
                                    then "inline-block;"
                                    else "none;") ++ 
                                "height:" ++ (show $ height-25) ++ "px;" ++
                                "width:calc(100% - 5px);" ++
                                "background-color : #1c1c1c;")
                               
        (termEl,_) <- elDynAttr' "div" dynTermAttrs $ blank
        
        
        let reload = ffilter (=="Get") eExec
        
        let initEv = ffilter (==Term) $ leftmost [updated $ nubDyn dType, tag (current dType) postBuild,tag (current dType) reload]
              
        performEvent $ ffor (tag (current dCombine) initEv) $ \(id,_,_,_) -> do
            liftIO $ setInnerHTML (_el_element  termEl) (Just "")
            jsId <- liftIO $ toJSVal ("t" ++ show id)
            liftIO $ term jsId
        
        return ()
            
    let ev = leftmost
           [
                
           ]
           
    return (termEl,ev)

frame :: MonadWidget t m 
        => MVar Int
        -> Event t ()
        -> Event t String
        -> Dynamic t (Int,String,WindowType,Double)
        -> Dynamic t String
        -> Dynamic t WindowType
        -> m (El t,Event t (Windows -> Windows))
frame refId postBuild eGet dCombine dName dType = do
    rec dAttrsFrame <- forDyn dCombine $ \(_,_,windowType,height) ->
            "class" =: "windowFrame"
         <> "spellcheck" =: "false"
         <> "oncontextmenu" =: "return false;"
          -- for firefox
         <> "style" =: ("height : " ++ show (height-24) ++ "px;"
                     ++ "width : calc(100% - 5px);"
                     ++ "box-sizing : border-box;"
                     ++ "outline: none;" 
                     ++ "display : " ++ if height <= 25 || windowType /= Frame
                                        then "none;" 
                                        else "inline-block;")

        let ev = tag (constant ()) $ ffilter (\(_,_,windowType,_) -> windowType == Frame) $ leftmost [tag (current dCombine) eGet, tag (current dCombine) postBuild]
        
        dir <- mapDyn unlines  =<< ls ev dName
        text <- commandAreaInput $ def & attributes .~ dAttrsFrame
                                       & font .~ constDyn "18px Arial"
                                       & setValue .~ (updated dir)
        let evOpenFile = attach (current dName) (_commandAreaInput_execute text)
        performEvOpenFile <- performEvent $ ffor evOpenFile $ \(vPath,vName) -> do
                            vId <- liftIO $ readMVar refId
                            liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                            return (vPath ++ vName,vId) 
                                  
    let ev = leftmost
           [
            ffor performEvOpenFile $ \(vName,vId) model ->
                if last vName == '/'
                then window_new Frame vName vId model
                else window_new File vName vId model
           ]
    elFrame <- wrapElement defaultDomEventHandler $ castToElement $ _commandAreaInput_element text
    return (elFrame, ev)
    
-- Update
    
update :: MonadWidget t m
          => MVar Int
          -> Windows
          -> Event t ()
          -> Event t ()
          -> Event t ()
          -> Event t (Windows -> Windows)
          -> Event t Window
          -> m (Dynamic t [Window])    
update refId model eNewWindow eNewDir eNewTerm eWindows eAdd = do          
    dWindowResize <- windowDimensions

    eId <- performEvent $ ffor (leftmost [eNewWindow,eNewDir,eNewTerm]) $ \_ -> do
                    vId <- liftIO $ readMVar refId
                    liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                    return vId
    dId <- holdDyn 0 eId            
    dynWindows <- foldDyn ($) model $ mergeWith (.)
             [
               fmap acmeWeb_resize $ updated dWindowResize
              ,eWindows
              ,fmap (window_new File "") (tagDyn dId eNewWindow)
              ,fmap (window_new Frame "/") (tagDyn dId eNewDir)
              ,fmap (window_new Term "Term") (tagDyn dId eNewTerm)
              ,fmap window_add eAdd
             ]
    mapDyn _windows dynWindows
    

acmeWeb_resize :: (Int,Int) -> Windows -> Windows
acmeWeb_resize (width,height) model =
  let colHeight = foldl (\acc col -> if (_window_height col) <= 25
                                     then let wHeight = 25-((_window_height col)/(fromIntegral (_client_height model)) * (fromIntegral (height-48)))
                                          in if (height-48) > (fromIntegral (_client_height model))
                                             then acc - wHeight
                                             else acc - wHeight
                                     else acc) (realToFrac $ height-48) (_windows model)
      helper oldHeight acc col = acc ++ [ if (_window_height col) > 25
                                          then col & window_height .~ ((_window_height col)/(fromIntegral oldHeight) * colHeight)
                                          else col]
  in Windows width (height-48) $ foldl (helper (_client_height model)) [] (_windows model)
  
window_delete :: Int -> [Window] -> [Window]
window_delete vId vWindows =
  if length vWindows == 1
  then []
  else 
    let helperFindIndex window = vId == _window_id window
        (Just index) = findIndex helperFindIndex vWindows;
    in if index /= 0
     then
       let vWindow = vWindows !! index
           vLastWindow = vWindows !! (index-1)
           newLastWindow = vLastWindow & window_height .~ ((_window_height vLastWindow) + (_window_height vWindow))
       in  take (index-1) vWindows ++ [newLastWindow] ++ drop (index+1) vWindows
     else
       let vWindow = vWindows !! index
           vNextWindow = vWindows !! (index+1)
           newNextWindow = vNextWindow & window_height .~ ((_window_height vNextWindow) + (_window_height vWindow))
       in  [newNextWindow] ++ drop (index+2) vWindows


max_window :: [Window] -> Window
max_window vWindows =
  foldl1 helper vWindows
     
  where helper acc x =
          if _window_height x > _window_height acc
          then x
          else acc
               
window_new :: WindowType -> String -> Int -> Windows -> Windows
window_new  vType vName vId model =
  let vWindows = _windows model
  in 
    if length vWindows == 0
    then model & windows .~ [Window vId Nothing vName vType (fromIntegral (_client_height model))]
    else
        let vMax = max_window vWindows
            helperFindIndex window = _window_id vMax == _window_id window
            (Just vMaxI) = findIndex helperFindIndex vWindows;
        in model & windows .~
            (take vMaxI vWindows ++
             [
               vMax & window_height .~ 25
             , Window vId Nothing vName vType ((_window_height vMax)-25)
             ] ++  drop (vMaxI+1) vWindows)
             
window_add :: Window ->  Windows -> Windows
window_add  vWindow model =
  let vWindows = _windows model
  in 
    if length vWindows == 0
    then model & windows .~ [vWindow & window_height .~ (fromIntegral (_client_height model))]
    else
        let vMax = max_window vWindows
            helperFindIndex window = _window_id vMax == _window_id window
            (Just vMaxI) = findIndex helperFindIndex vWindows;
        in model & windows .~
            (take vMaxI vWindows ++
             [
               vMax & window_height .~ 25
             , vWindow & window_height .~ ((_window_height vMax)-25)
             ] ++  drop (vMaxI+1) vWindows)

wr_height = 300
window_min_height = 25
                    
min_windowl :: [Window] -> Window
min_windowl vWindows =
  let vWindow = foldl helper Nothing vWindows
  in case vWindow of
       Nothing -> head vWindows
       (Just x) -> x
                   
  where helper acc x =
            if acc == Nothing && _window_height x > window_min_height
            then Just x
            else acc

min_windowr :: [Window] -> Window
min_windowr vWindows =
  let vWindow = foldr helper Nothing vWindows
  in case vWindow of
       Nothing -> head vWindows
       (Just x) -> x
                   
  where helper x acc =
            if acc == Nothing && _window_height x > window_min_height
            then Just x
            else acc
       
window_resize_lock  :: Int -> Windows -> Windows
window_resize_lock vId model =
    let vWindows = _windows model
        helperFindIndex window = vId == _window_id window
        (Just index) = findIndex helperFindIndex vWindows;
        vWindow = vWindows !! index
        lock = _window_lock vWindow
    in case lock of
            Nothing -> model
            Just coords ->  model & windows .~ (window_resize vId coords vWindows)

window_resize_unlock :: (Int,(Int,Int)) -> Windows -> Windows
window_resize_unlock (vId,(mx,my)) model =
    let vWindows = _windows model
        helperFindIndex window = vId == _window_id window
        (Just index) = findIndex helperFindIndex vWindows;
        vWindow = vWindows !! index
        lock = _window_lock vWindow
    in
        case lock of
            Nothing -> model
            (Just (x,y)) ->
           {-     let diffx = abs (mx-x)
                    diffy = abs (my-y)
                in  if diffx > 5 && diffy > 5 -}
                  --  then
                         let newWindow = vWindow & window_lock .~ Nothing
                             newWindows = take index vWindows ++ [newWindow] ++ drop (index+1) vWindows
                         in model & windows .~ newWindows
                  --  else model
                    
window_resize :: Int -> (Int,Int) -> [Window] -> [Window]
window_resize vId coords vWindows =
    let helperFindIndex window = vId == _window_id window
        (Just index) = findIndex helperFindIndex vWindows;
        vWindow = vWindows !! index
    in
      if index == 0
      then
          let nextWindow = min_windowl $ tail vWindows
              helperFindIndex window = _window_id nextWindow == _window_id window
              (Just nextWindowI) = findIndex helperFindIndex vWindows;  
              newNextWindowHeight =
                  let newWidth = (_window_height nextWindow)-wr_height
                  in if newWidth >= window_min_height
                     then newWidth
                     else window_min_height
              diffNextWidth = (_window_height nextWindow) - newNextWindowHeight
              newNextWindow = nextWindow & window_height .~ newNextWindowHeight           
              newWindow = vWindow & window_height .~
                           (if newNextWindowHeight > window_min_height
                            then (_window_height vWindow)+wr_height
                            else (_window_height vWindow)+diffNextWidth)
          in [newWindow] ++ take (nextWindowI-1) (tail vWindows) ++ [newNextWindow] ++ drop nextWindowI (tail vWindows)
      else if index /= (length vWindows) -1
        then
          let lastWindow = min_windowr $ take index vWindows
              helperFindIndex window = _window_id lastWindow  == _window_id window
              (Just lastWindowI) = findIndex helperFindIndex vWindows;  
              newLastWindowHeight =
                  let newWidth = (_window_height lastWindow)-(wr_height/2)
                  in if newWidth >= window_min_height
                     then newWidth
                     else window_min_height
              newLastWindow = lastWindow & window_height .~ newLastWindowHeight
              diffLastWidth = (_window_height lastWindow) - newLastWindowHeight
              nextWindow = min_windowl $ drop (index+1) vWindows
              helperFindIndex' window = _window_id nextWindow == _window_id window
              (Just nextWindowI) = findIndex helperFindIndex' vWindows;  
              newNextWindowHeight =
                  let newWidth = (_window_height nextWindow)-(wr_height/2)
                  in if newWidth >= window_min_height
                     then newWidth
                     else window_min_height
              diffNextWidth = (_window_height nextWindow) - newNextWindowHeight
              newNexWindow = nextWindow & window_height .~ newNextWindowHeight                                     
              newWindow = vWindow & window_lock .~ (Just coords)
                                  & window_height .~
                            (if newLastWindowHeight > window_min_height && newNextWindowHeight > window_min_height
                               then (_window_height vWindow)+wr_height
                            else if newLastWindowHeight <= window_min_height && newNextWindowHeight <= window_min_height
                               then (_window_height vWindow)+diffLastWidth+diffNextWidth
                            else if newLastWindowHeight <= window_min_height
                               then (_window_height vWindow)+(wr_height/2)+diffLastWidth
                            else (_window_height vWindow)+(wr_height/2)+diffNextWidth)
          in take lastWindowI vWindows ++
             [newLastWindow] ++
             take ((index-lastWindowI)-1) (drop (lastWindowI+1) vWindows) ++
             [newWindow] ++
             take ((nextWindowI-index)-1) (drop (index+1) vWindows) ++
             [newNexWindow] ++
             drop (nextWindowI+1) vWindows 
      else
          let lastWindow = min_windowr $ init vWindows
              helperFindIndex window = _window_id lastWindow  == _window_id window
              (Just lastWindowI) = findIndex helperFindIndex vWindows;  
              newLastWindowHeight =
                  let newWidth = (_window_height lastWindow)-wr_height
                  in if newWidth >= window_min_height
                     then newWidth
                     else window_min_height
              diffLastWidth = (_window_height lastWindow) - newLastWindowHeight
              newLastWindow = lastWindow & window_height .~ newLastWindowHeight                                     
              newWindow = vWindow & window_lock .~ (Just coords)
                                  & window_height .~
                           (if newLastWindowHeight > window_min_height
                            then (_window_height vWindow)+wr_height
                            else (_window_height vWindow)+diffLastWidth)
          in take lastWindowI (init vWindows) ++ [newLastWindow] ++ drop (lastWindowI+1) (init vWindows) ++ [newWindow]

