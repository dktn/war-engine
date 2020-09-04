module UnitParserSpec (spec) where

import           Universum

import qualified Data.Text                      as T

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

import           Arbitrary
import           Unit
import           UnitParser

spec :: Spec
spec = do
  describe "unitId" $ do
    it "parses division+regiment" $ property $
      \(NonEmptyAlphaNum d) (NonEmptyAlphaNum r) ->
        parseUnitId (d <> "/" <> r)
        == Right (UnitId (T.strip d) (T.strip r) Nothing)
    it "parses division/regiment/battalion" $ property $
      \(NonEmptyAlphaNum d) (NonEmptyAlphaNum r) (NonEmptyAlphaNum b) ->
        parseUnitId (d <> "/" <> r <> "/" <> b)
        == Right (UnitId (T.strip d) (T.strip r) (Just $ T.strip b))
