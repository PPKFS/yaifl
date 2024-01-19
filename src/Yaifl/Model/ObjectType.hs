module Yaifl.Model.ObjectType
  ( isType
  , makeTypeDAG
  ) where

import Yaifl.Model.Metadata
import Solitude
import Yaifl.Model.Kinds.Object
import Effectful.Optics
import qualified Data.Set as S

-- | Determine whether an object is of a certain type. This is separate to anything on Haskell's side
-- and the type system.
isType ::
  WithMetadata es
  => Is k A_Getter
  => LabelOptic' "objectType" k o ObjectType
  => o -- ^ The object.
  -> ObjectType -- ^ The type.
  -> Eff es Bool
isType o = isTypeInternal (o ^. #objectType)
  where
    isTypeInternal ::
      WithMetadata es
      => ObjectType
      -> ObjectType
      -> Eff es Bool
    isTypeInternal obj e' = do
      td <- use $ #typeDAG % at obj
      case td of
        Nothing -> noteError (const False) ("Found no type entry for " <> show obj)
        Just iv ->
          if
            e' `S.member` iv || obj == e'
          then
            return True
          else
            anyM (`isTypeInternal` e') iv

makeTypeDAG :: Map ObjectType (Set ObjectType)
makeTypeDAG = fromList
  [ ("object", fromList [])
  , ("thing", fromList ["object"])
  , ("room", fromList ["object"])
  -- probably useless because we don't have first class directions
  , ("direction", fromList [])
  , ("container", fromList ["thing"])
  , ("supporter", fromList ["thing"])
  , ("backdrop", fromList ["thing"])
  , ("person", fromList ["animal"])
  , ("animal", fromList ["thing"])
  -- same as direction, probably useless
  , ("region", fromList [])
  , ("door", fromList ["thing"])
  -- we also haven't (yet) got concepts
  ]