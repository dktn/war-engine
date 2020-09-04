{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Unit where

import           Universum

import           Control.Lens.TH (makeFieldsNoPrefix)

import           Symbol          (FromSymbol (..), ToSymbol (..))


data UnitSize =
  UnitSize
    { _unitBaseSize     :: UnitBaseSize
    , _unitSizeModifier :: Maybe UnitSizeModifier
    }
  deriving stock (Show, Eq, Ord)

data UnitBaseSize =
    Squadron
  | Company
  | Battalion
  | TaskForce
  | Regiment
  | CombatCommand
  | Brigade
  | Group
  | Corps
  deriving stock (Show, Eq, Ord)

unitBaseSizeSymbol :: UnitBaseSize -> Text
unitBaseSizeSymbol Squadron      = "Sq"
unitBaseSizeSymbol Company       = "I"
unitBaseSizeSymbol Battalion     = "II"
unitBaseSizeSymbol TaskForce     = "TF"
unitBaseSizeSymbol Regiment      = "III"
unitBaseSizeSymbol CombatCommand = "CC"
unitBaseSizeSymbol Brigade       = "X"
unitBaseSizeSymbol Group         = "Gp"
unitBaseSizeSymbol Corps         = "XXX"

instance ToSymbol UnitBaseSize where
  toSymbol = unitBaseSizeSymbol

data UnitSizeModifier =
    Minus
  | Plus
  deriving stock (Show, Eq, Ord)

unitSizeModifierSymbol :: UnitSizeModifier -> Text
unitSizeModifierSymbol Minus = "-"
unitSizeModifierSymbol Plus  = "+"

data UnitType =
    TankType
  | ArtilleryType
  | MechanizedVehicleType
  | MechanizedInfantryType
  | InfantryType
  deriving stock (Show, Eq, Ord)

isMechanized :: UnitType -> Bool
isMechanized InfantryType = False
isMechanized _            = True

isVehicle :: UnitType -> Bool
isVehicle MechanizedInfantryType = False
isVehicle InfantryType           = False
isVehicle _                      = True

data UnitId =
  UnitId
    { _division  :: Text
    , _regiment  :: Text
    , _battalion :: Maybe Text
    }
  deriving stock (Show, Eq, Ord)

formatUnitId :: UnitId -> Text
formatUnitId (UnitId d r Nothing ) = d <> "/" <> r
formatUnitId (UnitId d r (Just b)) = d <> "/" <> r <> "/" <> b

data Morale =
    Green
  | Veteran
  | Elite
  deriving stock (Show, Eq, Ord)

-- todo: type safe tank quality, stacking value, confict side, color, setup/arrival
data Unit =
  Unit
    { _name     :: Text      -- todo: make enum
    , _unitType :: UnitType
    , _unitSize :: UnitSize
    , _unitId   :: UnitId
    , _attack   :: Int
    , _defense  :: Int
    , _movement :: Int
    , _stacking :: Int
    , _morale   :: Morale
    , _setup    :: Maybe Text -- todo: type
    , _arrival  :: Maybe Text -- todo: type
    }
  deriving stock (Show, Eq, Ord)


-- todo: parsing
example :: Unit
example =
  Unit
    { _name     = "Infantry"
    , _unitType = InfantryType
    , _unitSize = UnitSize Battalion Nothing
    , _unitId   = UnitId "4" "8" (Just "2")
    , _attack   = 2
    , _defense  = 2
    , _movement = 3
    , _stacking = 1
    , _morale   = Veteran
    , _setup    = Nothing
    , _arrival  = Just "3-AB"
    }

makeFieldsNoPrefix ''UnitSize
makeFieldsNoPrefix ''UnitId
makeFieldsNoPrefix ''Unit
