module UnitParser where

import           Universum hiding (some)

import qualified Data.Text            as T

import           Text.Megaparsec      (Parsec, ParseErrorBundle, count, optional, parse, sepBy1, some, try, (<?>))
import           Text.Megaparsec.Char (alphaNumChar, char, space)

import           Unit                 hiding (unitId)

type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void
{-
Inf-II-id:4/8/2-a:2-d:2(v)-m:3-s:1-arr:(3-AB)
Inf-1.II.2-2(v)-3.a(3-AB).4/8/2
Art.Gp.S-(2)-1.s(2023).VIII/402 FA
type[-stacking].size[(mod)].attack-defense-movement.(a:arr|s:setup).id

Inf II [1] [2 2-v 3] [i:3-AB] [4/8/2]
Tank (Pzkw V) II [1] [7 4-e 6] [a:47Pz] [2 Pz/3/1]
Art Gp [4] [S (2) 1] [h:2023] [VIII/402 FA]

setup:
  i 3-AB   - turn-entry points
  a 47Pz   - starting area
  h 2023   - starting hex

configurable language
type-stacking size [attack defense movement] [setup] [unit id]
-}

testUnit1, testUnit2, testUnit3 :: Text
testUnit1  = "Inf                   [II] [1] [2 2   3] [i|3-AB] [4/8/2]"
testUnit2  = "Tank|Pzkw V (Panther) [II] [1] [7 4|e 6] [a|47Pz] [2 Pz/3/1]"
testUnit3  = "Art                   [Gp] [4] [S (2) 1] [h|2023] [VIII/402 FA]"

unit :: Text -> Text
unit unit = unit

unitId :: Parser UnitId
unitId = do
  space
  division <- some alphaNumChar <?> "division"
  space *> char '/' *> space
  regiment <- some alphaNumChar <?> "regiment"
  space
  battalion <- optional $ do
    char '/' *> space
    some alphaNumChar <?> "battalion"
  space
  pure $ UnitId (T.pack division) (T.pack regiment) (T.pack <$> battalion)

unitIdElem :: Parser UnitId
unitIdElem = do
  unitIdVal <- unitId
  pure unitIdVal

parseUnitId :: Text -> Either ParseError UnitId
parseUnitId = parse unitId "Cannot parse unit id"
