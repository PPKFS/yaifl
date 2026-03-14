module Yaifl.MultiLocated.Query
  ( setMultiLocated
  , modifyMultiLocated
  , updateMultiLocatedObject
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.TH

import Yaifl.MultiLocated.Kind
import Yaifl.Enclosing.Kind
import Yaifl.ObjectLike
import Yaifl.Metadata
import Yaifl.Enclosing.Query
import Yaifl.Move
import Yaifl.Tag
import qualified Data.Set as S

makeModify ''MultiLocated

updateMultiLocatedObject ::
  WMWithProperty wm MultiLocated
  => WMWithProperty wm Enclosing
  => WithoutMissingObjects wm es
  => ThingLike wm tl
  => tl
  -> Eff es ()
updateMultiLocatedObject tl = do
  t <- getThing tl
  case getMultiLocatedMaybe t of
    Nothing -> noteError (const ()) "the object had no multilocated component"
    Just ml -> mapM_ (\x -> do
      obj <- getEnclosingObject x
      let enc = getEnclosing obj
      updateToContain (getTaggedObject obj) enc t) (S.toList $ ml ^. #locations)