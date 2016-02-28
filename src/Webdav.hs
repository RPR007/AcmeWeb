module Webdav where

import Reflex.Dom
import Data.Default
import Data.Maybe
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.Text as T
import Text.HTML.TagSoup
import qualified Data.Char as Char

url :: String -> String
url path = "http://light9.ddns.net/fs" ++ path

webdavReq :: String -> String -> Maybe String -> XhrRequest
webdavReq verb url _data =
    let config =
            XhrRequestConfig { _xhrRequestConfig_headers = ("Content-Type" =: "text/xml; charset=UTF-8")
                             , _xhrRequestConfig_user = Nothing
                             , _xhrRequestConfig_password  = Nothing
                             , _xhrRequestConfig_responseType  = Nothing
                             , _xhrRequestConfig_sendData  = _data
                             }
    in xhrRequest verb url config
                
req :: MonadWidget t m
       => Event t ()
       -> String
       -> Dynamic t String
       -> Dynamic t (Maybe String)
       -> m (Dynamic t String)
req ev verb dUrl dData = do
  dCombine <- combineDyn (\a b -> (a,b)) dUrl dData
  xhr <- mapDyn (\(url,_data) -> webdavReq verb url _data) dCombine
  asyncEvent <- performRequestAsync $ tagDyn xhr ev
  send <- holdDyn Nothing $ fmap _xhrResponse_body asyncEvent
  mapDyn (fromMaybe "" . fmap T.unpack) send
  

urlDecode :: String -> Maybe String
urlDecode [] = Just []
urlDecode ('%':xs) =
  case xs of
    (a:b:xss) ->
      urlDecode xss
      >>= return . ((Char.chr . Prelude.read $ "0x" ++ [a,b]) :)
    _ -> Nothing
urlDecode ('+':xs) = urlDecode xs >>= return . (' ' :)
urlDecode (x:xs) = urlDecode xs >>= return . (x :)

                    
ls :: MonadWidget t m => Event t () -> Dynamic t String -> m (Dynamic t [String])
ls ev dPath = do
    dUrl <- mapDyn url dPath
    dXml <- req ev "PROPFIND" dUrl (constDyn Nothing)
    forDyn dXml $ \xml ->
        let tags = parseTags xml
        in if xml == ""
           then []
             else let xmlSection = fmap (tail . (takeWhile (~/= "</D:response>"))) $ sections (~== "<D:response>") tags
                      name = fmap (fromFooter . (take 2)) $ fmap (head . (sections (~== "<D:displayname>"))) xmlSection
                      _type = fmap (fromFooter . (take 2)) $ fmap (head . (sections (~== "<D:resourcetype>"))) xmlSection
                      parse = zipWith (\name _type -> if not (null _type)
                                                      then name ++ "/"
                                                      else name) name _type
                  in 
                    if not (null parse)
                    then ffor (tail parse) $ \str ->
                            case (urlDecode str) of
                                Nothing -> str
                                (Just decodeStr) -> decodeStr
                    else ffor parse $ \str ->
                            case (urlDecode str) of
                                Nothing -> str
                                (Just decodeStr) -> decodeStr
 where
   fromFooter [] = ""
   fromFooter x =
       case (last x) of
         (TagClose _) -> ""
         (TagOpen x _) -> x
         (TagText x) -> x
       
{-
mkdir ::

rmdir ::
    
touch ::

rm :: -}

read :: MonadWidget t m => Event t () -> Dynamic t String -> m (Dynamic t String)
read ev dPath = do
    dUrl <- mapDyn url dPath
    let ev' = tag (constant ()) $ ffilter (/="") $ tagDyn dPath ev
    req ev' "GET" dUrl (constDyn Nothing)
            
write :: MonadWidget t m => Event t String -> Dynamic t String -> m (Dynamic t String)
write ev dPath = do
  _data <- holdDyn "" ev
  _data' <- mapDyn Just _data
  dUrl <- mapDyn url dPath
  let ev' = tag (constant ()) (updated _data') 
  dRep <- req ev' "PUT" dUrl _data'
  forDyn dRep $ \rep ->
        if rep == ""
        then ""
        else
            let tags = parseTags rep
                xmlTitle = fmap (takeWhile (~/= "</title>")) $ sections (~== "<title>") tags
            in 
                if xmlTitle /= []
                    then fromFooter $ last $ head xmlTitle
                    else "Error"
  
  where
   fromFooter x =
       case x of
         (TagText x) -> x
