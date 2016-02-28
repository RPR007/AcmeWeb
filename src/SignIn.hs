{-# LANGUAGE RecursiveDo,DeriveGeneric,JavaScriptFFI, CPP #-}

module SignIn where

import Reflex.Dom
import Data.Default
import Data.Map (Map)
import Data.Monoid
import Data.Either
import Data.Aeson
import GHC.Generics
import Data.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import Data.Text
import Data.Time.Clock

import Control.Monad.IO.Class

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Storage

#ifdef __GHCJS__
foreign import javascript unsafe "location.href=$1" setHref :: JSVal -> IO ()
#else
setHref = error "setHref: only available from JavaScript"
#endif


#ifdef __GHCJS__
foreign import javascript unsafe "location.reload()" reload :: IO ()
#else
reload = error "reload: only available from JavaScript"
#endif

data Signin = Signin {
    password :: String
} deriving (Show, Eq, Read, Generic)

instance ToJSON Signin
instance FromJSON Signin

data Response = Response {
      ok :: Bool,
      r_error :: String
} deriving (Show, Eq, Read, Generic)

instance ToJSON Response
instance FromJSON Response

-- Utils

dynShow :: MonadWidget t m => String -> Dynamic t Bool -> m ()
dynShow t s = dynText =<< mapDyn (\s -> if s then t else "") s

ifFocus :: Bool -> String -> String -> String
ifFocus foc val def =
  if foc == False && val == def
  then ""
  else if foc && val == ""
    then def
    else val
    
inputSS :: MonadWidget t m => String -> Dynamic t (Map String String) -> String -> m (TextInput t,Dynamic t String)
inputSS initVal attrs type'  = do
  rec elClass "div" "info" $ dynShow initVal $ _textInput_hasFocus _input
      dynInput <- combineDyn (\a b -> (a,b)) (_textInput_hasFocus _input) (value _input)
      evInput <- return $ tag (current dynInput) (updated $ _textInput_hasFocus _input)
      _input <- textInput $ def & attributes .~ attrs
                                & textInputConfig_initialValue .~ initVal
                                & textInputConfig_inputType .~ type'
                                & setValue .~ fmap (\(foc,val) -> ifFocus foc val initVal) evInput
      valInput <- return $ _textInput_value _input
  return (_input,valInput)

setLogin :: String -> IO ()
setLogin val =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return ()
       Just win ->
         do Just storage <- getLocalStorage win
            setItem storage "login" $ show True
            setItem storage "username" val


isLogin :: IO Bool
isLogin =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return False
       Just win ->
         do Just storage <- getLocalStorage win
            item <- getItem storage "login"
            case item of
                Nothing -> return False
                Just item -> return $ ((read item) :: Bool)
                                  
getLogin :: IO String
getLogin =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return ""
       Just win ->
         do Just storage <- getLocalStorage win
            Just item <- getItem storage "username"
            return item

setLogout :: IO ()
setLogout =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return ()
       Just win ->
         do Just storage <- getLocalStorage win
            setItem storage "login" $ show False
            removeItem storage "username"
            
-- Sign In

signupReq :: [String] -> XhrRequest
signupReq par = xhrRequest "GET" ("php/signin.php?par=" ++  (BS.unpack $ encode $ makeSignin par)) def

makeSignin :: [String] -> Signin
makeSignin signin = Signin { password = signin !! 0 }
                             
inputPassword :: MonadWidget t m =>  Event t () -> m (Dynamic t [Either String String])
inputPassword signinEv = do
      -- Error box
  rec passwordErrorDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                       then "style" =: "display:block;"
                                       else "style" =: "display:none;") password
      passwordErrorEvAttr <- holdDyn ("style" =: "display:none;") $ leftmost [tagDyn passwordErrorDynAttr signinEv,
                                                                                 tagDyn (constDyn $ "style" =: "display:none")
                                                                                        (updated $ _textInput_hasFocus i_password)
                                                                                ]
      passwordErrorAttr <- combineDyn (<>) (constDyn $ "class" =: "error") passwordErrorEvAttr
      errDyn <- mapDyn (\(x:xs) -> if isLeft x then let (Left err) = x in err else "") password
      err <- holdDyn "" $ tagDyn errDyn signinEv
      elDynAttr "div" passwordErrorAttr $ dynText err

      -- Input
      passwordInputDynAttr <- mapDyn (\x -> if isLeft (Prelude.head x)
                                   then "style" =: "border: 1px solid red;border-radius : 3px;"
                                   else "style" =: "") password
      passwordInputEvAttr <- holdDyn ("style" =: "") $ tagDyn passwordInputDynAttr signinEv
      passwordInputAttr <- combineDyn (<>) (constDyn $ "class" =: "password"
                                                     <> "maxlength" =: "30") passwordInputEvAttr
                        
      (i_password,password) <- do
           (_input,dyn) <- inputSS "Password" passwordInputAttr "password"
           newDyn <- mapDyn requirements dyn
           return (_input,newDyn)
  return password

  where
    requirements str
        | str == "" = [Left "please enter a password"]
        | str == "Password" = [Left "please enter a password"]
        | otherwise = [Right str]
        
send :: MonadWidget t m =>
        Dynamic t [Either String String] ->
        m (Event t (), Dynamic t (Maybe Response))
send password = do
  -- View
  elClass "div" "info" $ blank
  signinEv <- signInButton
  elClass "div" "info" $ blank

  -- valid req
  rep <- mconcatDyn [password]
  validRep <- mapDyn (\x -> if isLeft (sequence x) then False else True)  rep
  req <- mapDyn (\x -> if isLeft (sequence x)
                       then signupReq [""]
                       else let (Right par) = (sequence x)
                            in signupReq par) rep --served by nginx
  -- Send to server       
  let signinEv' = gate (current validRep) signinEv
  asyncEvent <- performRequestAsync (tagDyn req signinEv')
  buttonDyn <- holdDyn Nothing $ fmap _xhrResponse_body asyncEvent
  serverRep <- mapDyn (fromMaybe "" . fmap Data.Text.unpack) buttonDyn
  parseRep <- mapDyn (\x -> (decode $ BS.pack x) :: Maybe Response) serverRep
  
  -- What to do with the Response
  performEvent $ ffor (updated parseRep) $ \rep -> liftIO $ Prelude.putStrLn $ show rep
  performEventAsync $ ffor (updated parseRep) receive
  return (signinEv,parseRep)
  
signInButton :: MonadWidget t m => m (Event t ())
signInButton = do
  (e, _) <- elAttr' "button" ("class" =: "signin-button") $ text "SIGN IN"
  return $ domEvent Click e
 
receive Nothing _ = return ()
receive (Just rep) cb =
  if ok rep
  then do
    liftIO $ setLogin "nemesis15"
    jsLocation <- liftIO $ toJSVal "/AcmeWeb"
    liftIO $ setHref jsLocation
  else return ()
  
accessDenied :: MonadWidget t m => Dynamic t (Maybe Response) -> m ()
accessDenied  dynRep =  do
  rec dynAttr <- holdDyn ("style" =: "display:none") $ leftmost [ev1,ev2]
      attr <-  mconcatDyn [dynAttr,(constDyn $ "class" =: "denied")]
      elDynAttr "div" attr  $ text "Access Denied"
                  
      ev1 <- (mapDyn display' dynRep) >>= \x -> return $ updated x
      ev2 <- (delay (fromInteger 1 :: NominalDiffTime) ev1)
             >>= \x -> return $ tagDyn (constDyn $ "style" =: "display:none") x
  return ()
         
  where
    display' Nothing = "style" =: "display:none"
    display' (Just par) =
        if ok par
        then "style" =: "display:none"
        else "style" =: "display:block;"
        