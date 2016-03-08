{-# LANGUAGE JavaScriptFFI, CPP  #-}

module Ace where

import GHCJS.Types
import GHCJS.Foreign
    
import Reflex.Dom
import Control.Monad.IO.Class

import Foreign.Ptr
    
#ifdef __GHCJS__
foreign import javascript unsafe
            "ace.require('ace/ext/language_tools');\
             var editor = ace.edit(($1).toString());\
             editor.setTheme('ace/theme/merbivore_soft');\
             editor.getSession().setUseWrapMode(true);\
             editor.setOptions({\
                enableBasicAutocompletion: true,\
                enableSnippets: true,\
                enableLiveAutocompletion: true\
             });\
             $r = editor"
            ace :: JSVal -> IO (Ptr a)

foreign import javascript unsafe
            "var modelist = ace.require('ace/ext/modelist');\
             var filePath = null;\
             var mode = modelist.getModeForPath('.' + $2).mode;\
             $1.session.setMode(mode);"
            aceMode :: Ptr a -> JSVal -> IO ()
            
foreign import javascript unsafe
            "$1.destroy();"
            aceDestroy :: Ptr a -> IO ()
                         
foreign import javascript unsafe
            "$1.resize();"
            aceResize :: Ptr a -> IO ()
                         
foreign import javascript unsafe
            "$1.setValue($2);"
            aceSetValue :: Ptr a -> JSVal -> IO ()

foreign import javascript unsafe
            "$r = $1.getValue();"
            aceGetValue :: Ptr a -> IO JSVal

foreign import javascript unsafe
            "$1.gotoLine($2,$3,$4);"
            aceGotoLine :: Ptr a -> Int -> Int -> Bool -> IO ()

foreign import javascript unsafe
            "$1.gotoPageUp()"
            acePageUp :: Ptr a -> IO ()
#else
ace = error "ace: only available from JavaScript"
aceMode = error "aceMode: only available from JavaScript"
aceDestroy = error "aceDestroy: only available from JavaScript"
aceResize = error "aceResize: only available from JavaScript"
aceSetInsert = error "aceSetValue: only available from JavaScript"
aceGetValue = error "aceGetValue: only available from JavaScript"
aceGotoLine = error "aceGotoLine : only available from javasScript"
acePageDown = error "acePageDown : only available from javasScript"
#endif
