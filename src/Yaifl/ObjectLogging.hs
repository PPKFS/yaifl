module Yaifl.ObjectLogging
  (
  Prettify(..)
  , shortPrint
  , objectName
  ) where

import Yaifl.Prelude
import Yaifl.Types
import Yaifl.ObjectLookup
import Text.Pretty.Simple (pString)

class Prettify o where
  prettify :: o -> Text

instance {-# OVERLAPPABLE #-} Prettify s where
  prettify = const "No prettify instance"

instance Prettify o => Prettify (Maybe o) where
  prettify Nothing = "Nothing"
  prettify (Just s) = prettify s

instance Prettify Text where
  prettify = id
instance Prettify (Object s d) where
  prettify Object{..} = _objName <> " (ID: " <>  show (unID _objID) <> ")\n" <> toStrict (pString (toString s)) where
    s = "{ Description = " <> _objDescription <>
        ", Type = " <> prettify _objType <>
        -- F.% ", Creation Time = " F.% F.stext
        ", Specifics = " <> prettify _objSpecifics <>
        ", Data = " <> prettify _objData

instance Prettify ObjType where
  prettify = unObjType

instance Prettify (Either a b) where
  prettify = either prettify prettify

instance Prettify [a] where
  prettify e = mconcat $ map prettify e

shortPrint
  :: Object s d
  -> Text
shortPrint Object{..} = _objName <> " (ID: " <>  show (unID _objID) <> ")"
{-
logObject
  :: MonadReader ObjectLike s o
  => Text
  -> o
  -> m ()
logObject n e = do
  o <- getObject e
  logVerbose $ n <> "\n" <> prettify o
  whenJust o $ \Object{..} -> logVerbose _objName
-}
objectName
  :: NoMissingObjects s m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m Text
objectName o = do
  o' <- getObject o
  return $ _objName o'