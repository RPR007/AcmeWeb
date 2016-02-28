{-# LANGUAGE ScopedTypeVariables, LambdaCase, ConstraintKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, RecursiveDo, GADTs, DataKinds, RankNTypes, TemplateHaskell #-}

module Utils where
    
import Reflex.Dom
import Reflex.Dom.Class

import Prelude hiding (mapM, mapM_, sequence, sequence_)
import Reflex
import Reflex.Host.Class
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Dependent.Sum (DSum (..))
import Data.Foldable
import Data.Traversable
import Control.Monad.Trans
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.State hiding (state, mapM, mapM_, forM, forM_, sequence, sequence_)
import qualified Control.Monad.State.Strict as S hiding (mapM, mapM_, forM, forM_, sequence, sequence_, get)
import GHCJS.DOM.Node
import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM (event, EventM, stopPropagation)
import GHCJS.DOM.Document
import GHCJS.DOM.Element as E
import GHCJS.DOM.Types hiding (Event)
import qualified GHCJS.DOM.Types as DOM (Event)
import GHCJS.DOM.NamedNodeMap as NNM
import Control.Lens hiding (element, children)
import Data.These
import Data.Align
import Data.Maybe
import Data.GADT.Compare.TH
import Data.Bitraversable
import GHCJS.DOM.MouseEvent
import Data.IORef
import Data.Default
import Data.List
import Data.Monoid
import qualified Data.Map as Map

import Control.Monad.Fix

wSimpleList :: (MonadWidget t m,Show a) => IORef [((Int, b), WidgetHost m (),Event t (WidgetHost m ()))] -> Dynamic t [(Int,a)] -> Dynamic t [(Int,a)] -> (Dynamic t a -> m b) -> m (Dynamic t [b])
wSimpleList stateRef allVals vals mkChild = do
    wList stateRef allVals vals (\_ dyn -> mkChild dyn) 

wList :: (MonadWidget t m,Show a) => IORef [((Int, b), WidgetHost m (),Event t (WidgetHost m ()))] -> Dynamic t [(Int,a)] -> Dynamic t [(Int,a)] -> (Int -> Dynamic t a -> m b) -> m (Dynamic t [b])
wList stateRef allVals vals mkChild = do
  postBuild <- getPostBuild
  let ev = leftmost [tagDyn vals (updated allVals), tagDyn vals postBuild]
  wListHold stateRef ev $ \id val -> do
                       mapVals <- mapDyn Map.fromList allVals
                       let evVal = Reflex.select (fanMap $ tagDyn mapVals ev) $ Const2 id
                       dVal <- holdDyn val evVal
                       mkChild id dVal


wListHold :: MonadWidget t m => IORef [((Int, b), WidgetHost m (),Event t (WidgetHost m ()))] -> Event t [(Int,a)] -> (Int -> a -> m b) -> m (Dynamic t [b])
wListHold stateRef ev mkChild = do
  doc <- askDocument
  endPlaceholder <- text' ""
  (newChildren, newChildrenTriggerRef) <- newEventWithTriggerRef
  runWidget <- getRunWidget
  let buildChild df id child = runWidget df $ wrapChild id child
      wrapChild :: MonadWidget t m => Int -> m a -> m (Int,a)
      wrapChild id child  = do
        w <- elAttr "div" ("id" =: show id) $ child
        return (id,w)
  
  children <- holdDyn [] newChildren
  childrenEv <- forDyn children $ \xs -> mergeWith (>>) (map snd xs)
  performEvent $ switchPromptlyDyn  childrenEv

  addVoidAction $ ffor (attachDyn children ev) $ \(curState,vals) -> do
      state <- liftIO $ readIORef stateRef 
      let curStateId = map (fst . fst) curState
      let valsId = map fst vals
      
                  
      let del = foldl (\acc x -> acc ++
                           case lookup x vals of
                              Nothing -> [(x,Nothing)]
                              _ -> []) [] curStateId
          newVals = map (\(a,b) -> (a,Just b)) vals
      (newState, postBuild) <- flip runStateT (return ()) $ liftM listMaybe $ forM (newVals) $ \(id,maybeVal) ->
           case maybeVal of
               Just val -> do
                 let idVals = map fst vals
                 let idCurState = map (\((a,_),_,_) -> a) state
                 let Just index = elemIndex id idVals
                            
                 el <- getElementById doc (show id)
                 
                 case el of
                   Nothing -> do
                     Just df <- createDocumentFragment doc
                     let ch = mkChild id val
                     (childResult, childPostBuild, childVoidAction) <- lift $ buildChild df id ch
                     modify (>>childPostBuild)
                     mp <- getParentNode endPlaceholder
                     forM_ mp $ \p -> insertBefore p (Just df) (Just endPlaceholder)
                     return $ Just (childResult,childPostBuild,childVoidAction)
                   (Just e) -> do
                     let index' = if index > 0 then index-1 else index
                     let state' = lookup' id state
                     let iDchildBefore = idVals !! index'
                     
                     if index' == 0 || index' == (length newVals)-1
                        then do
                          mp <- getParentNode endPlaceholder
                          forM_ mp $ \p -> insertBefore p el (Just endPlaceholder)
                     else do
                        (Just childBefore) <- getElementById doc (show iDchildBefore)
                        nextSibling <- getNextSibling childBefore
                        mp <- getParentNode endPlaceholder
                        forM_ mp $ \p -> insertBefore p el nextSibling
                        
                     case state' of
                        (Just ((id,_),childPostBuild,childVoidAction)) -> return $ state'
                        Nothing -> do
                            liftIO $ putStrLn "Hello World"
                            return Nothing
               Nothing -> do
                  return Nothing    
      let modifyState newState curSate = 
                let newStateId = map (\((a,_),_,_) -> a) newState
                    curStateId = map (\((a,_),_,_) -> a) state
                    helper acc x = 
                        let xId = (\((a,_),_,_) -> a) $ x 
                        in acc ++ 
                            (if elem xId curStateId
                                then []
                                else [x])
                in curSate ++ foldl helper [] newState
      liftIO $ modifyIORef stateRef (modifyState newState)
      runFrameWithTriggerRef newChildrenTriggerRef (map (\(a,_,c) -> (a,c)) newState)
      postBuild
  mapDyn (fmap (snd . fst)) children

lookup' :: (Eq a) => a -> [((a,b), c,d)] -> Maybe ((a,b) ,c,d)
lookup' _key []          =  Nothing
lookup'  key (((x,y),z,k):xyzks)
    | key == x          =  Just ((x,y),z,k)
    | otherwise         =  lookup' key xyzks
                           
listMaybe :: [Maybe a] -> [a]
listMaybe xs = foldl helper [] xs
   where helper acc x = acc ++
             case x of
               Nothing -> []
               (Just x) -> [x]
                           
