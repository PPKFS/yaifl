module Yaifl.Utils
    (
        doUntilJustM
      , doIfExists2
    ) where

import Yaifl.Prelude
import Yaifl.Common
import Colog

doUntilJustM :: (Foldable t, Monad m) => (a1 -> m (Maybe a2)) -> t a1 -> m (Maybe a2)
doUntilJustM f = runMaybeT . asumMap (MaybeT . f)

doIfExists2 :: WithGameLog w m => Maybe t1 -> Maybe t2 -> Text -> Text -> (t1 -> t2 -> World w m Bool) -> World w m Bool
doIfExists2 c1 c2 err1 err2 f = do
    when (isNothing c1) $ logError err1 
    when (isNothing c2) $ logError err2
    case (c1, c2) of
        (Just jc1, Just jc2) -> f jc1 jc2
        _ -> return False


{-
whenJustM_ :: Monad m => m (Maybe a) -> (a -> m ()) -> ()
whenJustM_ f = do
    _ <- whenJustM f
    pass

doIfExists :: HasMessageBuffer x => Maybe t -> Text -> (t -> StateT x Identity Bool) -> StateT x Identity Bool
doIfExists c1 err1 f = do
    when (isNothing c1) $ sayDbg err1 
    case c1 of
        Just jc1 -> f jc1
        _ -> return False


doIfExists3 :: HasMessageBuffer x => Maybe t1 -> Maybe t2 -> Maybe t3 -> Text -> Text -> Text -> (t1 -> t2 -> t3 -> StateT x Identity Bool) -> StateT x Identity Bool
doIfExists3 c1 c2 c3 err1 err2 err3 f = do
    when (isNothing c1) $ sayDbg err1 
    when (isNothing c2) $ sayDbg err2
    when (isNothing c3) $ sayDbg err3
    case (c1, c2, c3) of
        (Just jc1, Just jc2, Just jc3) -> f jc1 jc2 jc3
        _ -> return False

zoomOut :: Monad m => StateT (b1, b2) m a -> b2 -> StateT b1 m a
zoomOut stabc b = StateT $ \a -> do 
    (c, (a', b')) <- runStateT stabc (a, b)
    pure (c, a')



whenJustM_ :: Monad m => m (Maybe a) -> (a -> m ()) -> ()
whenJustM_ f = do
    _ <- whenJustM f
    pass

ifMaybeEq :: Eq a => a -> Maybe a -> Bool
ifMaybeEq a = maybe False (a ==)

ifMaybe :: (a -> Bool) -> Maybe a -> Bool
ifMaybe = maybe False-}