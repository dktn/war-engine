module Arbitrary where

import           Universum

import           Data.Char                      (isAlphaNum)
import qualified Data.Text                      as T

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Modifiers


newtype NonEmptyAlphaNumText = NonEmptyAlphaNum { getNonEmptyAlphaNum :: Text }
 deriving ( Eq, Ord, Show, Read, Typeable)

instance Arbitrary NonEmptyAlphaNumText where
  arbitrary = NonEmptyAlphaNum `fmap` (arbitrary `suchThat` isTextNonEmptyAlphaNum)

  shrink (NonEmptyAlphaNum xs) =
    [ NonEmptyAlphaNum xs'
    | xs' <- shrink xs
    , isTextNonEmptyAlphaNum xs'
    ]

isTextNonEmptyAlphaNum :: Text -> Bool
isTextNonEmptyAlphaNum text = isTextNonEmpty stext && isTextAlphaNum stext
  where
    stext = T.strip text
    isTextNonEmpty = not . T.null
    isTextAlphaNum = T.all isAlphaNum
