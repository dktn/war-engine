module Symbol where

import Universum


class ToSymbol a where
  toSymbol :: a -> Text

class FromSymbol a where
  fromSymbol :: Text -> Maybe a
