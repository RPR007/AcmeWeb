{-# LANGUAGE PatternSynonyms, ForeignFunctionInterface,JavaScriptFFI,CPP,RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Colum where

import GHCJS.DOM.Element hiding (drop)
import qualified GHCJS.DOM.Element as El
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.EventM
import GHCJS.DOM.Node
import GHCJS.DOM.Document (createDocumentFragment)
import Control.Monad
import Control.Monad.IO.Class

import Reflex.Host.Class
import Reflex.Dom
import Reflex.Contrib.Window
import Data.Default
import Data.FileEmbed

import Nav
import Window hiding (_client_width,_client_height,acmeWeb_resize,update,ColumDrop)

import Data.Monoid
import Data.List
import qualified Data.Map as Map

import Control.Lens
import Data.Dependent.Sum (DSum (..))

import Data.IORef
import Control.Concurrent.MVar
import Control.Monad.Fix

-- Model
       
data Colum
   = Colum { _colum_id :: Int,
             _colum_width :: Double
           } deriving (Show,Eq)

data ColumDrop
    = ColumDrop { _colum_drag :: Bool
                 ,_colum_drop :: Bool
                } deriving (Eq)
                                  
        
data Colums
    =  Colums { _client_width :: Int
              , _client_height :: Int
              , _colums :: [Colum] }
      
data ColumsConfig t m b 
    = ColumsConfig { _columsConfig_id :: m (MVar Int)
                   , _columsConfig_newColum :: Event t ()
                   , _columsConfig_drop :: Event t Int
                   , _columsConfig_colums :: (MVar Int) -> m Colums
                   , _columsConfig_stateRef :: Maybe (IORef [((Int, (Event t (Windows -> Windows), Event t (Int,Window))),WidgetHost m (), Event t (WidgetHost m ()))])
                   }

      
instance (Reflex t,MonadWidget t m) => Default (ColumsConfig t m b) where
  def = ColumsConfig {
                       _columsConfig_id = liftIO $ newMVar (0 :: Int)
                     , _columsConfig_newColum = never
                     , _columsConfig_drop = never
                     , _columsConfig_colums = \refId -> do
                         windowDimensions <- windowDimensions
                         (width,height) <- sample $ current windowDimensions

                         id1 <- liftIO $ readMVar refId
                         liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                         id2 <- liftIO $ readMVar refId
                         liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                                
                         return $ Colums width height
                             [ Colum id1 $ (fromIntegral width)*(4/7)
                             , Colum id2 $ (fromIntegral width)*(3/7)
                             ]
                     , _columsConfig_stateRef = Nothing
                     }
  

-- View

colums :: (MonadWidget t m,MonadFix (WidgetHost m)) => ColumsConfig t m b -> m ()
colums (ColumsConfig refIdM eNewcol eDrop vColumsM stateRef) = do
  rec refId <- refIdM
      vColums <- vColumsM refId
      fupdate <- update refId vColums eNewcol eColums
      eColums <- do
                rec columsEv <- simpleList fupdate (colum (refId,stateRef,dColumWindows) eDrop eColumsDrop eWindowsAdd)
                    dynEv <- mapDyn (\evList -> leftmost $ map (\(a,_,_,_) -> a) evList) columsEv
                    dynEvColumsDrop <- mapDyn (\evList -> leftmost $ map (\(_,b,_,_) -> b) evList) columsEv
                    let eColumsDrop = switchPromptlyDyn dynEvColumsDrop
                    dynEvWindowAdd <- mapDyn (\evList -> leftmost $ map (\(_,_,c,_) -> c) evList) columsEv
                    let eWindowsAdd = switchPromptlyDyn dynEvWindowAdd
                    dDColumWindows <- mapDyn (\evList -> Map.fromAscList $ zip [(1::Int)..] $ map (\(_,_,_,d) -> d) evList) columsEv
                    dColumWindows <- mapDyn (\xs -> concat $ map snd $ Map.toList xs) (joinDynThroughMap dDColumWindows)
                    
                return $ (switchPromptlyDyn dynEv)
  return ()
              
colum :: (MonadWidget t m,MonadFix (WidgetHost m))
         => (MVar Int,Maybe (IORef [((Int, (Event t (Windows -> Windows), Event t (Int,Window))), WidgetHost m () ,Event t (WidgetHost m ()))]),Dynamic t [(Int,Window)])
         -> Event t Int
         -> Event t Int
         -> Event t (Int,Window)
         -> Dynamic t (Colum)
         -> m (Event t (Colums -> Colums), Event t Int, Event t (Int,Window), Dynamic t [(Int,Window)])
colum (refId,stateRef,dWindows) eDrop eColumsDrop eWindowAdd dColum = do
  rec   dId <- mapDyn _colum_id dColum
        currentId <- sample $ current dId
               
        dStyle <- mapDyn columStyle dColum
        dAttrs <- mapDyn (\style -> "class" =: "col"
                           <> "style" =: style) dStyle
                              
        (elColum,(eColum,eAddWindow,dWindowsList)) <- elDynAttr' "div" dAttrs $ do
            postBuild <- getPostBuild
                 
            (_,eDrag,eExec) <- nav (constDyn "")  "New Dir Term Delcol"
                     
            -- Event      
            isColumDrop <- isColumDrop eDrag eDrop
            dDrop <- holdDyn 0 eDrop
             
            let eColumResize = attach (current dId) $ tagDyn dDrop $ ffilter _colum_drop (updated $ isColumDrop)
            let eColumDelete = tag (current dId) $ ffilter (=="Delcol") eExec
  
    
            -- Window Event

            let eNewWindow = tag (constant ()) $ ffilter (=="New") eExec
            let eNewDir= tag (constant ()) $ ffilter (=="Dir") eExec
            let eNewTerm = tag (constant ()) $ ffilter (=="Term") eExec
            
            sId <- sample $ current dId
            let vWindow = if sId == 1
                          then \refId -> do
                            windowDimensions <- windowDimensions
                            (width,height) <- sample $ current windowDimensions

                            id1 <- liftIO $ readMVar refId
                            liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                                
                            return $ Windows width (height-48)
                                [ Window id1 Nothing "Term" Term ((fromIntegral (height-48)))
                                ]
                          else defWindows
            (eAddWindow,dWindowsList) <- wWindows $ def 
                               & windowsConfig_newWindow .~ eNewWindow
                               & windowsConfig_newDir .~ eNewDir
                               & windowsConfig_newTerm .~ eNewTerm
                               & windowsConfig_columId .~ dId
                               & windowsConfig_list .~ dWindows
                               & windowsConfig_id .~ (return refId)
                               & windowsConfig_drop .~ eDrop
                               & windowsConfig_columDrop .~ eColumsDrop
                               & windowsConfig_add .~ eWindowAdd
                               & windowsConfig_stateRef .~ stateRef
                               & windowsConfig_windows .~ vWindow

            ev <- return $ leftmost
               [
                fmap (\(vId,mx) model -> 
                          let vColums = _colums model
                              fColums _vColums = Colums (_client_width model) (_client_height model) _vColums
                          in fColums $ colum_resize vId mx vColums) eColumResize
               ,fmap (\vId model -> 
                          let vColums = _colums model
                              fColums _vColums = Colums (_client_width model) (_client_height model) _vColums
                          in fColums $ colum_delete vId vColums) eColumDelete
               ]
            return (ev,eAddWindow,dWindowsList)
            
  eColumDrop <- wrapDomEvent (_el_element elColum) (`on` El.drop) $ return ()  
  return (eColum,tagDyn dId eColumDrop,eAddWindow,dWindowsList)
        
columStyle :: Colum -> String
columStyle vColum = 
  let id = _colum_id vColum
      width = _colum_width vColum
      styleWidth = "width : " ++ show width ++ "px;"
  in if id == 0
     then styleWidth
     else styleWidth ++ "border-left : 1px solid black;"
       
                 
isColumDrop :: MonadWidget t m => Event t () -> Event t Int -> m (Dynamic t ColumDrop)
isColumDrop eDrag eDrop = do
    dyn <- foldDyn ($) (ColumDrop False False) $ mergeWith (.)
             [
               ffor eDrag $ \_ model ->
                   ColumDrop True False
              ,ffor eDrop $ \_ model ->
                  let eDrag = _colum_drag model
                  in if eDrag
                     then ColumDrop False True
                     else model
             ]
    return $ nubDyn dyn
          
-- Update

update :: MonadWidget t m
          => MVar Int
          -> Colums
          -> Event t ()
          -> Event t (Colums -> Colums)
          -> m (Dynamic t [Colum])    
update refId model eNewColum eColums = do          
    dWindowResize <- windowDimensions

    performENewColum <- performEvent $ ffor eNewColum $ \_ -> do
                            id <- liftIO $ readMVar refId
                            liftIO $ modifyMVar_ refId $ \x -> return (x+1)
                            return id
                        
    dynColums <- foldDyn ($) model $ mergeWith (.)
             [
               fmap acmeWeb_resize $ updated dWindowResize
             , eColums
             , fmap colum_new performENewColum
             ]
    
    mapDyn _colums dynColums
           
-- Resize

acmeWeb_resize :: (Int,Int) -> Colums -> Colums
acmeWeb_resize (width,height) model = 
  let helper oldWidth acc col = acc ++ [Colum (_colum_id col) ((_colum_width col)/(fromIntegral oldWidth) * (fromIntegral width))]
  in Colums width height $ foldl (helper (_client_width model)) [] (_colums model)

colum_resize :: Int -> Int -> [Colum] -> [Colum]
colum_resize vId mx vColums =
    let helperFindIndex colum = vId == _colum_id colum
        (Just index) = findIndex helperFindIndex vColums;
        vColum = vColums !! index
        dblMx = fromIntegral mx
        columx = foldl (\acc x -> acc + (_colum_width x)) 0  (take index vColums);
    in
        if dblMx > columx  && index > 0
          then 
            let lastColum = vColums !! (index-1)
                lastColumx = foldl (\acc x -> acc + (_colum_width x)) 0  (take (index-1) vColums);
                newlastColum = Colum (_colum_id lastColum) (dblMx-lastColumx)
                newCurrentColum = Colum (_colum_id vColum) ((_colum_width vColum) - (dblMx-columx))
            in (take (index-1) vColums) ++ [newlastColum,newCurrentColum] ++ (Data.List.drop (index+1) vColums)
        else if dblMx < columx   && index > 0
          then 
            let lastColum = vColums !! (index-1)
                lastColumx = foldl (\acc x -> acc + (_colum_width x)) 0  (take (index-1) vColums);
                newlastColum = Colum (_colum_id lastColum) ((_colum_width lastColum) - (((_colum_width lastColum)+lastColumx)-dblMx))
                newCurrentColum = Colum (_colum_id vColum) ((_colum_width vColum) + (((_colum_width lastColum)+lastColumx)-dblMx))
            in (take (index-1) vColums) ++ [newlastColum,newCurrentColum] ++ (drop (index+1) vColums)
        else vColums

colum_delete :: Int -> [Colum] -> [Colum]
colum_delete vId vColums =
  if vId > 0
  then
    let helperFindIndex colum = vId == _colum_id colum
        (Just index) = findIndex helperFindIndex vColums;
        vColum = vColums !! index
        vLastColum = vColums !! (index-1)
        newLastColum = Colum (_colum_id vLastColum) $ (_colum_width vLastColum) + (_colum_width vColum)
    in  take (index-1) vColums ++ [newLastColum] ++ drop (index+1) vColums
  else vColums


colum_new :: Int -> Colums -> Colums
colum_new  vId model =
  let vColums = _colums model
  in 
    if length vColums == 0
    then Colums (_client_width model) (_client_height model) [Colum vId $ fromIntegral $ _client_width model]
    else
        let lastColum = last vColums
        in Colums (_client_width model) (_client_height model) $
             (init vColums) ++
             [
               Colum (_colum_id lastColum) ((_colum_width lastColum)/2)
             , Colum vId ((_colum_width lastColum)/2)
             ] 

-- Lens

liftM concat $ mapM makeLenses
  [ ''ColumsConfig
  ]

