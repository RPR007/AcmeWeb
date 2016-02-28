module Reflex.Contrib.Window where 

import Reflex
import Reflex.Dom

import Data.Bitraversable
import Control.Monad.IO.Class
    
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.EventM
--import GHCJS.DOM.DOMWindow
    
-- Window Dimension
windowDimensions :: MonadWidget t m => m (Dynamic t (Int,Int))
windowDimensions = do
  wv <- askWebView
  resize <- wrapDomEvent wv (`on` resize) $ liftIO $ bisequence (getInnerWidth wv,  getInnerHeight wv)
  defaultWidth <- liftIO $ getInnerWidth wv
  defaultHeight <- liftIO $ getInnerHeight wv
  holdDyn (defaultWidth,defaultHeight) resize
