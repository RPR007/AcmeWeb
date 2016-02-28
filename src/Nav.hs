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
       -> m (Event t (Int,Int), Event t (), Event t String)
nav title text =
  elAttr "nav"  ("style" =: "border-bottom : 1px solid black;height : 25px;box-sizing:border-box;") $
    elAttr "table" (("style" =: "height : 25px;")) $
      el "tr" $ do
       -- First TD
       (eClick,eDrag) <- elAttr "td" ("style" =: "width: 10px; vertical-align: top;") $ do
         (dragstart,_) <- elAttr' "div" (  "class" =: "control"
                                   <> "draggable" =: "true") $ blank
         eClick <- wrapDomEvent (_el_element dragstart) (`on` click) $ Reflex.Dom.getMouseEventCoords
                                             
         eDragstart <- wrapDomEvent (_el_element dragstart) (`on` dragStart) $ 
                        event >>= (liftIO . dataTransfer)
         eDragstart' <- performEvent $ fmap return eDragstart
         return (eClick,eDragstart')
       -- Second TD
       elAttr "td" ("style" =: "vertical-align: top;width:1%;white-space:nowrap;") $ dynText title
       -- Third TD
       dynExec <- elAttr "td" ("style" =: "vertical-align: top;") $ do
          c <- commandInput $ def & font .~ constDyn "Bold 15px Arial"
                           & commandInputConfig_initialValue .~ text
                           & attributes .~ constDyn ("class" =: "command"
                                                  <> "oncontextmenu" =: "return false;")
          return $ _commandInput_execute c
       return (eClick,eDrag,dynExec)
