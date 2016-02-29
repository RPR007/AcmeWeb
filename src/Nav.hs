{-# LANGUAGE PatternSynonyms, ForeignFunctionInterface,JavaScriptFFI,CPP,RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Nav where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.MouseEvent
import Reflex.Dom
import Control.Monad.IO.Class
import Data.Monoid
    
import Command

#ifdef __GHCJS__
foreign import javascript unsafe
           "$1.dataTransfer.setData('text', '');"
            dataTransfer :: MouseEvent -> IO ()
#else
dataTransfer = error "dataTransfer: only available from JavaScript"
#endif

nav :: MonadWidget t m
       => Dynamic t String
       -> String
       -> m (Event t (), Event t (), Event t (), Event t String)
nav title text =
  elAttr "nav"  ("style" =: "border-bottom : 1px solid black;height : 25px;box-sizing:border-box;"
              <> "oncontextmenu" =: "return false;") $
    elAttr "table" (("style" =: "height : 25px;")) $
      el "tr" $ do
       -- First TD
       (eLeftClick,eRightClick,eDrag) <- elAttr "td" ("style" =: "width: 10px; vertical-align: top;") $ do
         (control,_) <- elAttr' "div" (  "class" =: "control"
                                   <> "draggable" =: "true") $ blank
         eClick <- wrapDomEvent (_el_element control) (`on` mouseUp) $ Command.getMouseEventCoords $ _el_element control
         let eRightClick = tagDyn (constDyn ()) $ ffilter (\(button,_) -> button == 2) eClick
         let eLeftClick = tagDyn (constDyn ()) $ ffilter (\(button,_) -> button == 0) eClick 
         eDragstart <- wrapDomEvent (_el_element control) (`on` dragStart) $ 
                        event >>= (liftIO . dataTransfer)
         eDragstart' <- performEvent $ fmap return eDragstart
         return (eLeftClick,eRightClick,eDragstart')
       -- Second TD
       elAttr "td" ("style" =: "vertical-align: top;width:1%;white-space:nowrap;") $ dynText title
       -- Third TD
       dynExec <- elAttr "td" ("style" =: "vertical-align: top;") $ do
          c <- commandInput $ def & font .~ constDyn "Bold 15px Arial"
                           & commandInputConfig_initialValue .~ text
                           & attributes .~ constDyn ("class" =: "command")
          return $ _commandInput_execute c
       return (eLeftClick,eRightClick,eDrag,dynExec)
