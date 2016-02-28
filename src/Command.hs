{-# LANGUAGE RecursiveDo, JavaScriptFFI, CPP, TypeFamilies, TemplateHaskell #-}

module Command where
    
import Reflex
import Reflex.Host.Class
import Reflex.Dom hiding (getMouseEventCoords)

import Data.Bitraversable
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Default
    
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.DOM
--import GHCJS.DOM.Document
--import GHCJS.DOM.DOMWindow
import GHCJS.DOM.HTMLInputElement as Input
import GHCJS.DOM.HTMLTextAreaElement as TextArea
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.MouseEvent
import Data.Map as Map
import Data.Maybe
    
import Control.Lens
import Data.Dependent.Sum (DSum (..))
    
#ifdef __GHCJS__
foreign import javascript unsafe
            "var canvas = document.createElement('canvas');\
             var context = canvas.getContext('2d');context.font = $2;\
             var metrics = context.measureText($1);\
             $r=metrics.width;" getTextWidth :: JSVal -> JSVal -> IO Int

#else
getTextWidth = error "getTextWidth: only available from JavaScript"
getFont = error "getFont: only available from JavaScript"
#endif


-- Event Mouse Return

getElementCoords :: IsElement self => self -> IO (Double,Double)
getElementCoords self = helper (return $ Just self) 0 0
  where
    helper :: IsElement self => IO (Maybe self) -> Double -> Double -> IO (Double, Double)
    helper self x y = do
      e <- self
      case e of
        Nothing ->  return (x,y)
        (Just e) -> do
          offsetLeft <- getOffsetLeft e
          scrollLeft <- getScrollLeft e
          clientLeft <- getClientLeft e
          let newx = x + offsetLeft - (fromIntegral scrollLeft) + clientLeft
          offsetTop <- getOffsetTop e
          scrollTop <- getScrollTop e
          clientTop <- getClientTop e
          let newy = y + offsetTop - (fromIntegral scrollTop) + clientTop
          helper (getOffsetParent e) newx newy

getMouseEventCoords :: (IsMouseEvent e, IsElement self) => self -> EventM MouseEvent e (Int, (Int, Int))
getMouseEventCoords e = do
  ev <- event
  (x,y) <- liftIO $ getElementCoords e
  (mx,my) <-mouseClientXY
  button <- liftIO $ getButton ev
  return (fromIntegral button, (mx - (round x), my - (round y)))
         
-- Command Input

                  
data CommandInput t
   = CommandInput { _commandInput_value :: Dynamic t String
                  , _commandInput_input :: Event t String
                  , _commandInput_keypress :: Event t Int
                  , _commandInput_keydown :: Event t Int
                  , _commandInput_keyup :: Event t Int
                  , _commandInput_hasFocus :: Dynamic t Bool
                  , _commandInput_execute :: Event t String
                  , _commandInput_element :: HTMLInputElement
                  }

data CommandInputConfig t
    = CommandInputConfig { _commandInputConfig_inputType :: String
                         , _commandInputConfig_initialValue :: String
                         , _commandInputConfig_setValue :: Event t String
                         , _commandInputConfig_setButton :: Dynamic t Int
                         , _commandInputConfig_font :: Dynamic t String
                         , _commandInputConfig_attributes :: Dynamic t (Map String String)
                         }

instance Reflex t => Default (CommandInputConfig t) where
  def = CommandInputConfig { _commandInputConfig_inputType = "text"
                           , _commandInputConfig_initialValue = ""
                           , _commandInputConfig_setValue = never
                           , _commandInputConfig_setButton = constDyn 2
                           , _commandInputConfig_font = constDyn "12px Arial"
                           , _commandInputConfig_attributes = constDyn mempty
                           }

commandInput :: MonadWidget t m => CommandInputConfig t -> m (CommandInput t)
commandInput (CommandInputConfig inputType initial eSetValue dButton dFont dAttrs) = do
  e <- liftM castToHTMLInputElement $ buildEmptyElement "input" =<< do
         dAttrsAddType <- mapDyn (Map.insert "type" inputType) dAttrs
         combinedAttrsdFont <- combineDyn (\attrs font -> (attrs,font)) dAttrsAddType dFont
         mapDyn (\(attrs,font) -> Map.insertWith (\font style -> style <> font) "style" ("font : " <> font <> ";") attrs) combinedAttrsdFont
  Input.setValue e (Just initial)
  performEvent_ $ fmap (Input.setValue e . Just) eSetValue
  eChange <- wrapDomEvent e (`on` input) $ fromMaybe "" <$> Input.getValue e
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- on e blurEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity False]
    unsubscribeOnfocus <- on e focusEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  dFocus <- holdDyn False eChangeFocus
  eKeypress <- wrapDomEvent e (`on` keyPress) getKeyEvent
  eKeydown <- wrapDomEvent e (`on` keyDown) getKeyEvent
  eKeyup <- wrapDomEvent e (`on` keyUp) getKeyEvent
  onClick <- wrapDomEvent e (`on` mouseUp) $ getMouseEventCoords e
  dValue <- holdDyn initial $ leftmost [eSetValue, eChange]
  -- Command
  let fCoords (button,buttonUp,_) = buttonUp == button
  let attachDButtonOnClick = attachDynWith (\a (b,c) -> (a,b,c)) dButton onClick
  let filterOnclick = ffilter fCoords attachDButtonOnClick
  coords <- foldDyn (\(_,_,(x1,x2)) _ -> (x1,x2)) (-1,-1) filterOnclick
  let eValueClick = tagDyn dValue filterOnclick
  dValueClick <- holdDyn "" eValueClick
  let attachDFontDvalueClick =  attachDyn dFont eValueClick
  wordsCoords <- performEvent $ fmap (\(font,val) -> wordsCoords font val) attachDFontDvalueClick
  let zipCoord = attachDynWith (\a b -> zip (word a) b) dValueClick wordsCoords
  let eExecute =  attachDynWith command coords zipCoord
  
  return $ CommandInput dValue eChange eKeypress eKeydown eKeyup dFocus eExecute e

wordsCoords :: MonadIO m => String -> String -> m [(Int,Int)]
wordsCoords font str = 
  let words = word str
  in do
    list <- sequence $ helper words
    return $ setCoords list

  where
    helper :: MonadIO m => [String] -> [m Int]
    helper [] = []
    helper (x:xs) =
      (do
      jsX <- liftIO $ toJSVal x
      jsFont <- liftIO $ toJSVal font
      liftIO $ getTextWidth jsX jsFont):(helper xs)

setCoords :: [Int] -> [(Int,Int)]
setCoords xs = Prelude.foldl helper [] xs
  where
    helper [] x = [(0,x)]
    helper acc x = let (_,lastx) = last acc
                   in acc ++ [(lastx,lastx+x)]

word :: String -> [String]
word [] = []
word (x:xs) =
  case x of
    ' ' -> getSpace : (word dropSpace)
    x -> getWord : (word dropWord)

    where 
      getWord =  takeWhile (\x -> x /=' ') (x:xs)
      dropWord = dropWhile (\x -> x /=' ') (x:xs)
      getSpace = takeWhile (\x -> x ==' ') (x:xs)
      dropSpace = dropWhile (\x -> x ==' ') (x:xs)
                  
command (x,_) words =
    Prelude.foldl (\acc (str,(x1,x2)) ->
               if x >= x1 && x <= x2
               then str
               else acc) "" words


------------------------------ Command Area --------------------------------

data CommandAreaInput t
   = CommandAreaInput {
                    _commandAreaInput_value :: Dynamic t String
                  , _commandAreaInput_input :: Event t String
                  , _commandAreaInput_keypress :: Event t Int
                  , _commandAreaInput_keydown :: Event t Int
                  , _commandAreaInput_keyup :: Event t Int
                  , _commandAreaInput_hasFocus :: Dynamic t Bool
                  , _commandAreaInput_execute :: Event t String
                  , _commandAreaInput_element :: HTMLTextAreaElement
                  }

data CommandAreaInputConfig t
    = CommandAreaInputConfig {
                           _commandAreaInputConfig_inputType :: String
                         , _commandAreaInputConfig_initialValue :: String
                         , _commandAreaInputConfig_setValue :: Event t String
                         , _commandAreaInputConfig_setButton :: Dynamic t Int
                         , _commandAreaInputConfig_font :: Dynamic t String
                         , _commandAreaInputConfig_attributes :: Dynamic t (Map String String)
                         }

instance Reflex t => Default (CommandAreaInputConfig t) where
  def = CommandAreaInputConfig {
                             _commandAreaInputConfig_inputType = "text"
                           , _commandAreaInputConfig_initialValue = ""
                           , _commandAreaInputConfig_setValue = never
                           , _commandAreaInputConfig_setButton = constDyn 2
                           , _commandAreaInputConfig_font = constDyn "12px Arial"
                           , _commandAreaInputConfig_attributes = constDyn mempty
                           }

commandAreaInput :: MonadWidget t m => CommandAreaInputConfig t -> m (CommandAreaInput t)
commandAreaInput (CommandAreaInputConfig inputType initial eSetValue dButton dFont dAttrs) = do
  e <- liftM castToHTMLTextAreaElement $ buildEmptyElement "textarea" =<< do
         dAttrsAddType <- mapDyn (Map.insert "type" inputType) dAttrs
         combinedAttrsdFont <- combineDyn (\attrs font -> (attrs,font)) dAttrsAddType dFont
         mapDyn (\(attrs,font) -> Map.insertWith (\font style -> style <> font) "style" ("font : " <> font <> ";") attrs) combinedAttrsdFont
  TextArea.setValue e $ Just initial
  performEvent_ $ fmap (TextArea.setValue e . Just) eSetValue
  eChange <- wrapDomEvent e (`on` input) $ fromMaybe "" <$> TextArea.getValue e
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eChangeFocus <- newEventWithTrigger $ \eChangeFocusTrigger -> do
    unsubscribeOnblur <- on e blurEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity False]
    unsubscribeOnfocus <- on e focusEvent $ liftIO $ do
      postGui $ runWithActions [eChangeFocusTrigger :=> Identity True]
    return $ liftIO $ unsubscribeOnblur >> unsubscribeOnfocus
  dFocus <- holdDyn False eChangeFocus
  eKeypress <- wrapDomEvent e (`on` keyPress) getKeyEvent
  eKeydown <- wrapDomEvent e (`on` keyDown) getKeyEvent
  eKeyup <- wrapDomEvent e (`on` keyUp) getKeyEvent
  onClick <- wrapDomEvent e (`on` mouseUp) $ getMouseEventCoords e
  dValue <- holdDyn initial $ leftmost [eSetValue, eChange]
  -- Command
  let fCoords (button,buttonUp,_) = buttonUp == button
  let attachDButtonOnClick = attachDynWith (\a (b,c) -> (a,b,c)) dButton onClick
  let filterOnclick = ffilter fCoords attachDButtonOnClick
  coords <- foldDyn (\(_,_,(x1,x2)) _ -> (x1,x2)) (-1,-1) filterOnclick
  let eValueClick = tagDyn dValue filterOnclick
  dValueClick <- holdDyn "" eValueClick
  let attachDFontDvalueClick =  attachDyn dFont eValueClick
  let mouseLine = ffor onClick $ \(b,(x,y)) -> (truncate ((fromIntegral y)/(21.5 :: Double)))+1
  --performEvent $ ffor mouseLine $ \x  -> liftIO $ putStrLn $ show x 
  dMouseLine <- holdDyn 0 mouseLine
  wordsCoords <- performEvent $ fmap (\(font,val) -> mapM (wordsCoords font) (lines val)) attachDFontDvalueClick
  let selectLine = fmap (\(val,line) -> (lines val) !! (line-1)) (attachDyn dValueClick mouseLine)
  dSelectLine <- holdDyn "" selectLine
  let selectWordsCoords = ffor (attachDyn dMouseLine wordsCoords) $ \(line,wordsCoords) -> wordsCoords !! (line-1)
  let zipCoord = attachDynWith (\a b -> zip (word a) b) dSelectLine selectWordsCoords
  let eExecute =  attachDynWith command coords zipCoord
  
  return $ CommandAreaInput dValue eChange eKeypress eKeydown eKeyup dFocus eExecute e


liftM concat $ mapM makeLenses
  [ ''CommandInputConfig
   ,''CommandAreaInputConfig
  ]

class HasFont a where
  type Font a :: *
  font :: Lens' a (Font a)

instance HasFont (CommandInputConfig t) where
  type Font (CommandInputConfig t) = Dynamic t String
  font = commandInputConfig_font

instance HasFont (CommandAreaInputConfig t) where
  type Font (CommandAreaInputConfig t) = Dynamic t String
  font = commandAreaInputConfig_font
         
class HasButton a where
  type Button a :: *
  setButton :: Lens' a (Button a)

instance HasButton (CommandInputConfig t) where
  type Button (CommandInputConfig t) = Dynamic t Int
  setButton = commandInputConfig_setButton

instance HasButton (CommandAreaInputConfig t) where
  type Button (CommandAreaInputConfig t) = Dynamic t Int
  setButton = commandAreaInputConfig_setButton
              
instance HasAttributes (CommandInputConfig t) where
  type Attrs (CommandInputConfig t) = Dynamic t (Map String String)
  attributes = commandInputConfig_attributes

instance HasAttributes (CommandAreaInputConfig t) where
  type Attrs (CommandAreaInputConfig t) = Dynamic t (Map String String)
  attributes = commandAreaInputConfig_attributes

instance HasSetValue (CommandInputConfig t) where
  type SetValue (CommandInputConfig t) = Event t String
  setValue = commandInputConfig_setValue
             
instance HasSetValue (CommandAreaInputConfig t) where
  type SetValue (CommandAreaInputConfig t) = Event t String
  setValue = commandAreaInputConfig_setValue

instance HasValue (CommandInput t) where
  type Value (CommandInput t) = Dynamic t String
  value = _commandInput_value

instance HasValue (CommandAreaInput t) where
  type Value (CommandAreaInput t) = Dynamic t String
  value = _commandAreaInput_value
