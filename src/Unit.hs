module Unit where

import Universum

import Symbol


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
