{-# LANGUAGE JavaScriptFFI, CPP  #-}

module Term where

import GHCJS.Types
import GHCJS.Foreign
    
import Reflex.Dom
import Control.Monad.IO.Class

import Foreign.Ptr
    
#ifdef __GHCJS__
foreign import javascript unsafe
            "var socket = io(location.origin, {path: '/wetty/socket.io'});\
             var buf = '';\
             var e = document.getElementById($1);\
             Terminal.colors[256] = '#1c1c1c';\
             Terminal.colors[257] = '#f0f0f0';\
             term = new Terminal({\
                cols: Math.floor(e.offsetWidth/10),\
                rows: Math.floor(e.offsetHeight/19),\
                useStyle: true,\
                screenKeys: true,\
                cursorBlink: false\
            });\
            function Wetty(argv) {\
                this.argv_ = argv;\
                this.io = null;\
                this.pid_ = -1;\
            };\
            Wetty.prototype.run = function() {\
                this.io = this.argv_.io.push();\
                this.io.onVTKeystroke = this.sendString_.bind(this);\
                this.io.sendString = this.sendString_.bind(this);\
                this.io.onTerminalResize = this.onTerminalResize.bind(this);\
            };\
            Wetty.prototype.onTerminalResize = function(col, row) {\
                socket.emit('resize', { col: col, row: row });\
            };\
            socket.on('connect', function() {\
            term.open(e);\
            socket.emit('resize', { col: Math.floor(e.offsetWidth/10), row: Math.floor(e.offsetHeight/19) });\
            $('#' + $1).resize(function() {\
                socket.emit('resize', { col: Math.floor(e.offsetWidth/10), row: Math.floor(e.offsetHeight/19) });\
                term.resize(Math.floor(e.offsetWidth/10),Math.floor(e.offsetHeight/19));\
            });\
            term.on('data', function(data) {\
                socket.emit('input', data);\
            });\
            if (buf && buf != '')\
            {\
                term.write(data);\
                buf = '';\
            }\
            });\
            socket.on('output', function(data) {\
                if (!term) {\
                    buf += data;\
                    return;\
                }\
                term.write(data);\
            });\
            socket.on('disconnect', function() {\
                term.write('Socket.io connection closed');\
                term.destroy();\
            });\
            $r = term"
            term :: JSVal -> IO (Ptr a)
       
foreign import javascript unsafe
            "$1.resize($2/10,$3/19);"
            termResize :: Ptr a -> Int -> Int -> IO ()
#else
term = error "term: only available from JavaScript"
termResize = error "termResize: only available from JavaScript"
#endif
