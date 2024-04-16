module BuildingDefs where


import Solitude hiding (Type)
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create.Object
import Yaifl.Model.WorldModel
import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query (isSubregionOf, areInRegion)
import qualified Data.List.NonEmpty as NE
import Yaifl.Model.Kinds.Direction
import Named hiding (Name)
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Create.RoomConnection
import Yaifl.Game.ObjectSpecifics (addDoor, WMHasObjSpecifics)
import Yaifl.Model.MultiLocated
import Language.Haskell.TH
import Data.Char (toUpper)

data Building wm = Building
  { name :: Text
  , floors :: NonEmpty (BuildingFloor wm)
  , buildingRegion :: RegionEntity
  }

data BuildingFloor wm = BuildingFloor
  { level :: Int
  , entrances :: Int
  , exits :: (RoomEntity, WMDirection wm)
  , floorRooms :: [RoomEntity]
  , floorRegion :: RegionEntity
  }
deriving stock instance Show (WMDirection wm) => Show (Building wm)
deriving stock instance Show (WMDirection wm) => Show (BuildingFloor wm)

{-
makePlan "Building"
  [ (ParallelSteps [(Step "name"), (Step "numberOfComponents")])
  , (Step "entryRoom")
  , (Step "base")
  , (IterateStep (BaseCase "entryRoom") "component")
  , (Step "building")
  ]

type BuildingPlan m i ba c b = BuildingPlan' m Int Text c ba c b

defBang :: Bang
defBang = Bang NoSourceUnpackedness NoSourceStrictness

data Step = Step String | ParallelSteps [Step] | IterateStep BaseCase String
data BaseCase = BaseCase String

makePlan :: String -> [Step] -> DecsQ
makePlan n steps = pure $ [DataD [] (mkName (n <> "Plan'")) ((PlainTV (mkName "m") BndrReq) : (map (\x -> PlainTV x BndrReq) $ getNamesFromSteps (reverse $ snd mkVars))) Nothing [recCon] []]
  where
    mkVars = foldl' (\(fields, steps') step -> ((mkStep steps' step) <> fields , step:steps')) ([], []) steps
    recCon = RecC (mkName (n <> "Plan")) $ reverse $ fst mkVars

mkStep :: [Step] -> Step -> [VarBangType]
mkStep prior (Step n) = [(mkName $ "make" <> (over _head toUpper n), defBang,
  foldl' (\t f -> AppT (AppT ArrowT f) t) (AppT ((VarT $ mkName "m")) $ VarT (mkName n)) (getTypesFromSteps prior)) ]
mkStep prior (ParallelSteps s) = reverse $ mconcat $ map (mkStep prior) s
mkStep prior (IterateStep (BaseCase b) n) = [(mkName $ "make" <> (over _head toUpper n), defBang,
  foldl' (\t f -> AppT (AppT ArrowT (if (mkName b) == f then AppT (ConT (mkName "NonEmpty")) (VarT f) else VarT f)) t) (AppT ((VarT $ mkName "m")) $ VarT (mkName n)) (getNamesFromSteps prior)) ]

getTypesFromSteps :: [Step] -> [Type]
getTypesFromSteps s = mconcat $ map go s
  where
    go :: Step -> [Type]
    go (Step n) = [VarT (mkName n)]
    go (IterateStep _ n) = [VarT (mkName n)]
    go (ParallelSteps n) = reverse $ mconcat $ map go n

getNamesFromSteps :: [Step] -> [Name]
getNamesFromSteps s = mconcat $ map go s
  where
    go :: Step -> [Name]
    go (Step n) = [(mkName n)]
    go (IterateStep _ n) = [(mkName n)]
    go (ParallelSteps n) = reverse $ mconcat $ map go n

-}