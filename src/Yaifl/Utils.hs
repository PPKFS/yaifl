module Yaifl.Utils
    (
        doIfExists, doIfExists2, doIfExists3,
        zoomOut
    ) where

import Yaifl.Say
import Relude
import Yaifl.Common

doIfExists :: HasMessageBuffer w => Maybe a -> Text -> (a -> System w Bool) -> System w Bool
doIfExists c1 err1 f = do
    when (isNothing c1) $ sayDbg err1 
    case c1 of
        Just jc1 -> f jc1
        _ -> return False

doIfExists2 :: HasMessageBuffer w => Maybe a -> Maybe b -> Text -> Text 
    -> (a -> b -> System w Bool) -> System w Bool
doIfExists2 c1 c2 err1 err2 f = do
    when (isNothing c1) $ sayDbg err1 
    when (isNothing c2) $ sayDbg err2
    case (c1, c2) of
        (Just jc1, Just jc2) -> f jc1 jc2
        _ -> return False

doIfExists3 :: HasMessageBuffer w => Maybe a -> Maybe b -> Maybe c -> Text -> Text 
    -> Text -> (a -> b -> c -> System w Bool) -> System w Bool
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