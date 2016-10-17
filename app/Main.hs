{-# LANGUAGE PatternSynonyms, ForeignFunctionInterface,JavaScriptFFI,CPP,RecursiveDo, ScopedTypeVariables, FlexibleContexts,TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Main where

import GHCJS.DOM.Element as El
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.EventM
import GHCJS.DOM.Node
import Data.Monoid
import Data.Default
import Control.Monad.IO.Class
    
import Reflex.Dom
import Data.FileEmbed

import Command
import Colum
import SignIn

import Data.IORef
import Control.Concurrent.MVar
import Control.Monad.Fix

main = mainWidget acmeWeb

acmeWeb :: (MonadWidget t m,MonadFix (WidgetHost m)) => m ()
acmeWeb = do
      isLogin <- liftIO $ isLogin
      if isLogin
        then do
         rec  (elAcmeWeb,_) <- elAttr' "div" (  "id" =: "acmeweb"
                           <> "class" =: "acmeweb") $ do
                  eNav <- nav
                  let eNewColum = tag (constant ()) $ ffilter (=="Newcol") eNav
                  let eLogout = tag (constant ()) $ ffilter (=="Logout") eNav
                  performEventAsync $ ffor eLogout (\n cb -> liftIO setLogout >> liftIO reload)
                  
                  eDrop <- wrapDomEvent (_el_element elAcmeWeb) (`on` El.drop) $ do
                                    preventDefault
                                    ev <- event
                                    liftIO $ getClientX ev
                  performEvent $ fmap return eDrop
                  eDragOver <- wrapDomEvent (_el_element elAcmeWeb) (`on` dragOver) $ preventDefault
                  performEvent $ fmap return eDragOver
                  
                  stateRef <- liftIO $ newIORef []
                  let refId = liftIO $ newMVar (0 :: Int)
                  colums $ def & columsConfig_newColum .~ eNewColum
                               & columsConfig_drop .~ eDrop
                               & columsConfig_id .~ refId
                               & columsConfig_stateRef .~ (Just stateRef)
         return ()
        else
            elClass "section" "signin-section" $ 
            elClass "div" "signin" $ do
                rec password <- inputPassword signinEv
                    accessDenied dynRep
                    (signinEv,dynRep) <- send password
                return ()
                   
nav :: MonadWidget t m => m (Event t String)
nav = el "nav" $ do
   elAttr "div" ("style" =: "display: inline-block;vertical-align:middle;width : 30px;border-bottom : 1px solid black;height : 22px;") $ blank
   c <- commandInput $ def & font .~ constDyn "Bold 15px Arial"
                           & commandInputConfig_initialValue .~ "Newcol Logout"
                           & attributes .~ constDyn ("class" =: "command"
                                                    <> "style" =: "border-bottom : 1px solid black;height : 22px;"
                                                    <> "oncontextmenu" =: "return false;")
   return $ _commandInput_execute c