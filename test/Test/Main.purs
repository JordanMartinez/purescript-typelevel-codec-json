module Test.Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Sum (enumSum)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (launchAff_)
import Simple.JSON (readImpl, writeImpl)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Codec.Argonaut (class IsJsonCodec, type (:::), TJsonCodec, Tarray, Tboolean, Teither, Tint, Tlist, Tmap, Tmaybe, TmaybeNull, Tnewtype, Tnumber, Tobject, Tstring, Ttuple, Tvariant, Using(..))
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-argonaut-codecs tests" do
    it "encode >>> decode == id" do
      decodeJson (encodeJson value) `shouldEqual` Right value
  describe "purescript-simple-json tests" do
    it "writeImpl >>> readImpl == id" do
      runExceptT (readImpl (writeImpl value)) `shouldEqual` (Identity (Right value))

{-
value :: Using EntireRecord EntireRecord                                       -}
value :: EntireRecord ::: EntireRecordCodec
value = Using exampleRecord

type EntireRecord =
  { string :: String
  , boolean :: Boolean
  , int :: Int
  , number :: Number
  , array :: Array String
  , record ::
    { foo :: String
    , bar :: Int
    , baz ::
      { baz1 :: String
      , baz2 :: Int
      }
  }
  , variant :: Variant
    ( foo :: String
    , bar :: Int
    )
  , maybe :: Maybe Int
  , maybeNull :: Maybe Int
  , either :: Either Int Int
  , list :: List Int
  , tuple :: Tuple Int Int
  , map :: Map Int Int
  , customType :: CustomType
  , newtype :: MyInt
  }

exampleRecord :: EntireRecord
exampleRecord =
  { string: "stringValue"
  , boolean: true
  , int: 4
  , number: 42.0
  , array: ["elem1", "elem2", "elem3"]
  , record: { foo: "bar", bar: 8, baz: { baz1: "simple", baz2: 8 } }
  , variant: V.inj (Proxy :: Proxy "foo") "variantValue"
  , maybe: Nothing
  , maybeNull: Nothing
  , either: Right 4
  , list: 5 : 4 : 3 : 2 : 1 : Nil
  , tuple: Tuple 8 7
  , map: Map.singleton 4 7
  , customType: Custom1
  , newtype: MyInt 4
  }

type EntireRecordCodec = Tobject "EntireRecord"
  ( string :: Tstring
  , boolean :: Tboolean
  , int :: Tint
  , number :: Tnumber
  , array :: Tarray Tstring
  , record :: Tobject "record"
    ( foo :: Tstring
    , bar :: Tint
    , baz :: Tobject "baz"
      ( baz1 :: Tstring
      , baz2 :: Tint
      )
    )
  , variant :: Tvariant
    ( foo :: Tstring
    , bar :: Tint
    )
  , maybe :: Tmaybe Tint
  , maybeNull :: TmaybeNull Tint
  , either :: Teither Tint Tint
  , list :: Tlist Tint
  , tuple :: Ttuple Tint Tint
  , map :: Tmap Tint Tint
  , customType :: TcustomType
  , newtype :: Tnewtype MyInt Tint
  )

data CustomType
  = Custom1
  | Custom2

derive instance eqCustomType :: Eq CustomType
instance showCustomType :: Show CustomType where
  show = case _ of
    Custom1 -> "Custom1"
    Custom2 -> "Custom2"

customTypeCodec :: JsonCodec CustomType
customTypeCodec = enumSum to from
  where
    to = case _ of
      Custom1 -> "Custom1"
      Custom2 -> "Cusotm2"

    from = case _ of
      "Custom1" -> Just Custom1
      "Custom2" -> Just Custom2
      _ -> Nothing

foreign import data TcustomType :: TJsonCodec
instance reflectCodecTcustomType :: IsJsonCodec TcustomType CustomType where
  reflectCodec _ = customTypeCodec

newtype MyInt = MyInt Int
derive newtype instance eqMyInt :: Eq MyInt
derive instance newtypeMyInt :: Newtype MyInt _
instance showMyInt :: Show MyInt where
  show (MyInt i) = "MyInt(" <> show i <> ")"
