{-# LANGUAGE JavaScriptFFI, CPP  #-}

module Reflex.Contrib.Window where 

import Reflex
import Reflex.Dom

import Data.Bitraversable
import Control.Monad.IO.Class
    
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.EventM
--import GHCJS.DOM.DOMWindow
    
#ifdef __GHCJS__
foreign import javascript unsafe
        "var screenPixelRatio = 0, zoomLevel = 0;\
        if(window.devicePixelRatio && supportFullCss3()) {\
            screenPixelRatio = window.devicePixelRatio;\
        } else if(window.screenX == '0') {\
            screenPixelRatio = (window.outerWidth - 8) / window.innerWidth;\
        } else\
        {\
            var scr = window.frames.screen;\
            screenPixelRatio = scr.deviceXDPI / scr.systemXDPI;\
        }\
        if (screenPixelRatio <= 0.11){ \
            zoomLevel = -7;\
        } else if (screenPixelRatio <= 0.25) {\
            zoomLevel = -6;\
        } else if (screenPixelRatio <= 0.33) {\
            zoomLevel = -5.5;\
        } else if (screenPixelRatio <= 0.40) {\
            zoomLevel = -5;\
        } else if (screenPixelRatio <= 0.50) {\
            zoomLevel = -4;\
        } else if (screenPixelRatio <= 0.67) {\
            zoomLevel = -3;\
        } else if (screenPixelRatio <= 0.75) {\
            zoomLevel = -2;\
        } else if (screenPixelRatio <= 0.85) {\
            zoomLevel = -1.5;\
        } else if (screenPixelRatio <= 0.98) {\
            zoomLevel = -1;\
        } else if (screenPixelRatio <= 1.03) {\
            zoomLevel = 0;\
        } else if (screenPixelRatio <= 1.12) {\
            zoomLevel = 1;\
        } else if (screenPixelRatio <= 1.2) {\
            zoomLevel = 1.5;\
        } else if (screenPixelRatio <= 1.3) {\
            zoomLevel = 2;\
        } else if (screenPixelRatio <= 1.4) {\
            zoomLevel = 2.5;\
        } else if (screenPixelRatio <= 1.5) {\
            zoomLevel = 3;\
        } else if (screenPixelRatio <= 1.6) {\
            zoomLevel = 3.3;\
        } else if (screenPixelRatio <= 1.7) {\
            zoomLevel = 3.7;\
        } else if (screenPixelRatio <= 1.8) {\
            zoomLevel = 4;\
        } else if (screenPixelRatio <= 1.9) {\
            zoomLevel = 4.5;\
        } else if (screenPixelRatio <= 2) {\
            zoomLevel = 5;\
        } else if (screenPixelRatio <= 2.1) {\
            zoomLevel = 5.2;\
        } else if (screenPixelRatio <= 2.2) {\
            zoomLevel = 5.4;\
        } else if (screenPixelRatio <= 2.3) {\
            zoomLevel = 5.6;\
        } else if (screenPixelRatio <= 2.4) {\
            zoomLevel = 5.8;\
        } else if (screenPixelRatio <= 2.5) {\
            zoomLevel = 6;\
        } else if (screenPixelRatio <= 2.6) {\
            zoomLevel = 6.2;\
        } else if (screenPixelRatio <= 2.7) {\
            zoomLevel = 6.4;\
        } else if (screenPixelRatio <= 2.8) {\
            zoomLevel = 6.6;\
        } else if (screenPixelRatio <= 2.9) {\
            zoomLevel = 6.8;\
        } else if (screenPixelRatio <= 3) {\
            zoomLevel = 7;\
        } else if (screenPixelRatio <= 3.1) {\
            zoomLevel = 7.1;\
        } else if (screenPixelRatio <= 3.2) {\
            zoomLevel = 7.2;\
        } else if (screenPixelRatio <= 3.3) {\
            zoomLevel = 7.3;\
        } else if (screenPixelRatio <= 3.4) {\
            zoomLevel = 7.4;\
        } else if (screenPixelRatio <= 3.5) {\
            zoomLevel = 7.5;\
        } else if (screenPixelRatio <= 3.6) {\
            zoomLevel = 7.6;\
        } else if (screenPixelRatio <= 3.7) {\
            zoomLevel = 7.7;\
        } else if (screenPixelRatio <= 3.8) {\
            zoomLevel = 7.8;\
        } else if (screenPixelRatio <= 3.9) {\
            zoomLevel = 7.9;\
        } else if (screenPixelRatio <= 4)   {\
            zoomLevel = 8;\
        } else if (screenPixelRatio <= 4.1) {\
            zoomLevel = 8.1;\
        } else if (screenPixelRatio <= 4.2) {\
            zoomLevel = 8.2;\
        } else if (screenPixelRatio <= 4.3) {\
            zoomLevel = 8.3;\
        } else if (screenPixelRatio <= 4.4) {\
            zoomLevel = 8.4;\
        } else if (screenPixelRatio <= 4.5) {\
            zoomLevel = 8.5;\
        } else if (screenPixelRatio <= 4.6) {\
            zoomLevel = 8.6;\
        } else if (screenPixelRatio <= 4.7) {\
            zoomLevel = 8.7;\
        } else if (screenPixelRatio <= 4.8) {\
            zoomLevel = 8.8;\
        } else if (screenPixelRatio <= 4.9) {\
            zoomLevel = 8.9;\
        } else if (screenPixelRatio <= 5) {\
            zoomLevel = 9;\
        }else {\
            zoomLevel = 10;\
        }\
        $r=zoomLevel"
        getZoom :: IO Double

#else
getZoom = error "getZoom: only available from JavaScript"
#endif

-- Window Dimension
windowDimensions :: MonadWidget t m => m (Dynamic t (Int,Int))
windowDimensions = do
  wv <- askWebView
  resizeWithZoom <- wrapDomEvent wv (`on` resize) $ do
    zoom <- liftIO $ getZoom
    liftIO $ bisequence (zoom, getInnerWidth wv,  getInnerHeight wv)
  let resize = fmap (\(_,x,y) -> (x,y)) $ ffilter (\(zoom,_,_) -> zoom == 0) resizeWithZoom
  defaultWidth <- liftIO $ getInnerWidth wv
  defaultHeight <- liftIO $ getInnerHeight wv
  holdDyn (defaultWidth,defaultHeight) resize
