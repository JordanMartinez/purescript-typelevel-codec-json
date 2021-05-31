-- | Provides a near 1-to-1 representation of `purescript-codec-argonaut`
-- | API surface.
-- |
-- | A few functions' corresponding representations were renamed
-- | to prevent possible confusion that can arise when the same
-- | name appears in multiple modules:
-- | - Data.Codec.Argonaut.Compat (maybe) -> `TmaybeNull`
-- | - Data.Codec.Argonaut.Compat (foreignObject) -> `TforeignObjectKeyProp`
-- |
-- | Other API has not been yet been implemented, mainly because I'm either
-- | not sure whether it can be done or is worth doing. If you need it,
-- | you should define your own `TmyNameHere` whose kind is `TJsonCodec`
-- | and implement its `IsJsonCodec` instance for your specific situation:
-- | - Data.Codec.Argonaut (indexedArray) - not sure how to approach this yet
-- |     because one typically uses it with in combination with an `Applicative`
-- | - Data.Codec.Argonaut (fix) - not sure how to do this
-- | - Data.Codec.Argonaut.Migration - all functions besides `renameField` -
-- |     not sure whether it's worth it.
-- | - Data.Codec.Argonaut (prop) - not sure whether it's worth it.
-- | - Data.Codec.Argonaut (prismaticCodec) - not sure whether it's worth it.
-- | - Data.Codec.Argonaut.Sum (enumSum, taggedSum) - not sure whether it's worth it.
-- |
-- | Other API does not need to be implemented:
-- | - Data.Codec.Argonaut (prop, record, recordProp) - this is handled by `Tobject`
-- | - Data.Codec.Argonaut.Record (record, object) - this is handled by `Tobject`
module Type.Codec.Json where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as Argonaut
import Data.Argonaut.Encode (class EncodeJson)
import Data.Bifunctor (bimap)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JPropCodec, JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Common as Common
import Data.Codec.Argonaut.Compat as Compat
import Data.Codec.Argonaut.Generic (class NullarySumCodec)
import Data.Codec.Argonaut.Generic as CGeneric
import Data.Codec.Argonaut.Migration as CMigrate
import Data.Codec.Argonaut.Variant as CVariant
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (CodePoint)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Foreign as Foreign
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList as RL
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A newtype that tracks at the type-level the value-level JSON codec
-- | to use to encode/decode a given value when developers are forced
-- | to use type-class-based codecs (e.g. `purescript-argonaut-codecs`
-- | and/or `purescript-simple-json`).
-- |
-- | Ideally, one would write `Using` in an infix position
-- | to make code more readable. For example...
-- | ```
-- | MyType `Using` MyCodec
-- | ```
-- | Unfortunately, the PureScript compiler fails to parse the above code.
-- | To workaround this issue, use the `:::` type alias instead
-- | ```
-- | type Routes =
-- |   { foo :: GET "/foo" {
-- |       response :: Using MyType MyCodec
-- |     }
-- |   , bar :: GET "/bar" {
-- |       response :: MyType ::: MyCodec
-- |     }
-- |   }
-- | ```
newtype Using :: Type -> TJsonCodec -> Type
newtype Using a codec = Using a

infixl 4 type Using as :::

derive newtype instance eqUsing :: Eq a => Eq (Using a codec)
derive newtype instance ordUsing :: Ord a => Ord (Using a codec)
derive newtype instance showUsing :: Show a => Show (Using a codec)
derive instance genericUsing :: Generic (Using a codec) _
derive instance newtypeUsing :: Newtype (Using a codec) _

instance encodeJsonAsJson :: IsJsonCodec codec a => EncodeJson (Using a codec) where
  encodeJson :: Using a codec -> Json
  encodeJson (Using val) = Codec.encode (reflectCodec _codec) val
    where
      _codec = Proxy :: Proxy codec

instance decodeJsonUsing :: IsJsonCodec codec a => DecodeJson (Using a codec) where
  decodeJson :: Json -> Either Argonaut.JsonDecodeError (Using a codec)
  decodeJson =
    bimap codecToArgonautJsonError Using <<< Codec.decode (reflectCodec _codec)
    where
      _codec = Proxy :: Proxy codec

-- | No error information is lost when converting `codec-argonaut`
-- | (value-level based codes) errors to `argonaut-codecs`
-- | (type-class based codecs) errors since they are
-- | isomorphic to one another.
codecToArgonautJsonError :: Codec.JsonDecodeError -> Argonaut.JsonDecodeError
codecToArgonautJsonError = case _ of
  Codec.TypeMismatch str -> Argonaut.TypeMismatch str
  Codec.UnexpectedValue json -> Argonaut.UnexpectedValue json
  Codec.AtIndex i err -> Argonaut.AtIndex i $ codecToArgonautJsonError err
  Codec.AtKey str err -> Argonaut.AtKey str $ codecToArgonautJsonError err
  Codec.Named str err -> Argonaut.Named str $ codecToArgonautJsonError err
  Codec.MissingValue -> Argonaut.MissingValue

instance writeForeignUsing :: IsJsonCodec codec a => WriteForeign (Using a codec) where
  writeImpl (Using a) = jsonToForeign $ Codec.encode (reflectCodec _codec) a
    where
      _codec = Proxy :: Proxy codec

      jsonToForeign :: Json -> Foreign.Foreign
      jsonToForeign = unsafeCoerce

instance readForeignUsing :: IsJsonCodec codec a => ReadForeign (Using a codec) where
  readImpl =
    ExceptT
      <<< Identity
      <<< bimap mapErr Using
      <<< Codec.decode (reflectCodec _codec)
      <<< foreignToJson
    where
      _codec = Proxy :: Proxy codec

      mapErr = NEL.singleton <<< codecToForeignError

      foreignToJson :: Foreign.Foreign -> Json
      foreignToJson = unsafeCoerce

-- | Some error information is lost when converting `codec-argonaut`
-- | (value-level based codes) errors to `argonaut-codecs`
-- | (type-class based codecs) errors since they both store
-- | the underlying errors differently.
-- |
-- | `AtKey` and `AtIndex` will be converted one-to-one. All
-- | other errors will be printed to a string and wrapped in
-- | `ForeignError`.
codecToForeignError :: Codec.JsonDecodeError -> Foreign.ForeignError
codecToForeignError = case _ of
  Codec.AtIndex i err -> Foreign.ErrorAtIndex i $ codecToForeignError err
  Codec.AtKey str err -> Foreign.ErrorAtProperty str $ codecToForeignError err
  otherErr -> Foreign.ForeignError $ Codec.printJsonDecodeError otherErr

-- | Converts a type-level representation of a value-level codec into
-- | its corresponding value-level codec.
-- |
-- | This class should be used when implementing `Using`'s
-- | instances for type-class-based codecs.
class IsJsonCodec :: TJsonCodec -> Type -> Constraint
class IsJsonCodec codec a | codec -> a where
  reflectCodec :: Proxy codec -> JsonCodec a

-- | Custom kind to indiciate a type-level representation of
-- | a value-level codec.
-- |
-- | In general, if a function/codec is called `foo` in
-- | `purescript-codec-argonaut`, that function will be called `Tfoo` here.
data TJsonCodec

-- | Same as `Data.Codec.Argonaut.json`
foreign import data Tjson :: TJsonCodec
instance reflectCodecJsonTjson :: IsJsonCodec Tjson Json where
  reflectCodec _ = Codec.json

-- | Same as `Data.Codec.Argonaut.null`
foreign import data Tnull :: TJsonCodec
instance reflectCodecUnitTnull :: IsJsonCodec Tnull Unit where
  reflectCodec _ = Codec.null

-- | Same as `Data.Codec.Argonaut.boolean`
foreign import data Tboolean :: TJsonCodec
instance reflectCodecBooleanTboolean :: IsJsonCodec Tboolean Boolean where
  reflectCodec _ = Codec.boolean

-- | Same as `Data.Codec.Argonaut.int`
foreign import data Tint :: TJsonCodec
instance reflectCodecIntTint :: IsJsonCodec Tint Int where
  reflectCodec _ = Codec.int

-- | Same as `Data.Codec.Argonaut.number`
foreign import data Tnumber :: TJsonCodec
instance reflectCodecNumberTnumber :: IsJsonCodec Tnumber Number where
  reflectCodec _ = Codec.number

-- | Same as `Data.Codec.Argonaut.string`
foreign import data Tstring :: TJsonCodec
instance reflectCodecStringTstring :: IsJsonCodec Tstring String where
  reflectCodec _ = Codec.string

-- | Same as `Data.Codec.Argonaut.codePoint`
foreign import data TcodePoint :: TJsonCodec
instance reflectCodecCodePointTcodePoint :: IsJsonCodec TcodePoint CodePoint where
  reflectCodec _ = Codec.codePoint

-- | Same as `Data.Codec.Argonaut.char`
foreign import data Tchar :: TJsonCodec
instance reflectCodecCharTchar :: IsJsonCodec Tchar Char where
  reflectCodec _ = Codec.char

-- | Same as `Data.Codec.Argonaut.jarray`
foreign import data Tjarray :: TJsonCodec
instance reflectCodeTjarrayArray :: IsJsonCodec Tjarray (Array Json) where
  reflectCodec _ = Codec.jarray

-- | Same as `Data.Codec.Argonaut.jobject`
foreign import data Tjobject :: TJsonCodec
instance reflectCodecTjobjectObject :: IsJsonCodec Tjobject (Object Json) where
  reflectCodec _ = Codec.jobject

-- | Same as `Data.Codec.Argonaut.void`
foreign import data Tvoid :: TJsonCodec
instance reflectCodecVoidTvoid :: IsJsonCodec Tvoid Void where
  reflectCodec _ = Codec.void

-- | Same as `Data.Codec.Argonaut.array`
foreign import data Tarray :: TJsonCodec -> TJsonCodec
instance reflectCodecArrayTarray :: IsJsonCodec codec a => IsJsonCodec (Tarray codec) (Array a) where
  reflectCodec _ = Codec.array (reflectCodec _codec)
    where
    _codec = Proxy :: Proxy codec

-- Note: implementing `indexedArray` would take a bit more work to get right
--   since one is expected to use an Applicative to further encode/decode
--   the value to/from an array
-- data TJIndexedCodec
-- foreign import data TindexedArray :: Symbol -> TJIndexedCodec -> TJsonCodec
-- foreign import data Tindex :: Nat -> TJsonCodec -> TJIndexedCodec
-- instance reflectCoecTindexedArraya :: IsJsonCodec (TindexedArray sym indexedCodec) a where
--   reflectCodec _ = Codec.indexedArray (reflectSymbol _sym) reflectIndexedCodec _indexedCodec
--     where
--     _sym = Proxy :: Proxy sym
--     _indexedCodec = Proxy :: Proxy indexedCodec

-- Note: not sure how to do this one yet.
-- -- | Same as `Data.Codec.Argonaut.fix`
-- foreign import data Tfix :: (TJsonCodec -> TJsonCodec) -> TJsonCodec
-- instance reflectCodecTfix :: IsJsonCodec codec a => IsJsonCodec (Tfix codecFunc) wrapper where
--   reflectCodec _ = Codec.fix \c -> ???
--     where
--     _codec = Proxy :: Proxy codec

-- | Unwraps/Wraps the newtype constructor before/after the codec on
-- | the underlying value runs
foreign import data Tnewtype :: Type -> TJsonCodec -> TJsonCodec
instance reflectCodecTnewtypeA :: (Newtype wrapper a, IsJsonCodec codec a) => IsJsonCodec (Tnewtype wrapper codec) wrapper where
  reflectCodec _ = _Newtype (reflectCodec _codec)
    where
    _codec = Proxy :: Proxy codec

-- | Same as `Data.Codec.Argonaut.Common.either`
foreign import data Teither :: TJsonCodec -> TJsonCodec -> TJsonCodec
instance reflectCodecTeitherABEither :: (IsJsonCodec codecA a, IsJsonCodec codecB b) => IsJsonCodec (Teither codecA codecB) (Either a b) where
  reflectCodec _ = Common.either (reflectCodec _codecA) (reflectCodec _codecB)
    where
    _codecA = Proxy :: Proxy codecA
    _codecB = Proxy :: Proxy codecB

-- | Same as `Data.Codec.Argonaut.Common.foreignObject`, encoding
-- | as an array of 2-element key-value arrays.
-- |
-- | ```
-- | [ ["a", 1]
-- | , ["b", 2]
-- | ]
-- | ```
foreign import data TforeignObject :: TJsonCodec -> TJsonCodec
instance reflectCodecTforeignObjectObject :: IsJsonCodec codec a => IsJsonCodec (TforeignObject codec) (Object a) where
  reflectCodec _ = Common.foreignObject (reflectCodec _codec)
    where
    _codec = Proxy :: Proxy codec

-- | Same as `Data.Codec.Argonaut.Compat.foreignObject`, where the keys
-- | are properties.
-- |
-- | ```
-- | { "a": 1, "b": 2 }
-- | ```
foreign import data TforeignObjectKeyProp :: TJsonCodec -> TJsonCodec
instance reflectCodecTforeignObjectKeyPropObject :: IsJsonCodec codec a => IsJsonCodec (TforeignObjectKeyProp codec) (Object a) where
  reflectCodec _ = Compat.foreignObject (reflectCodec _codec)
    where
    _codec = Proxy :: Proxy codec

-- | Same as `Data.Codec.Argonaut.Common.list`
foreign import data Tlist :: TJsonCodec -> TJsonCodec
instance reflectCodecTlistList :: IsJsonCodec codec a => IsJsonCodec (Tlist codec) (List a) where
  reflectCodec _ = Common.list (reflectCodec _codec)
    where
    _codec = Proxy :: Proxy codec

-- | Same as `Data.Codec.Argonaut.Common.map`
foreign import data Tmap :: TJsonCodec -> TJsonCodec -> TJsonCodec
instance reflectCodecTmapMap :: (Ord key, IsJsonCodec codecKey key, IsJsonCodec codecValue value) => IsJsonCodec (Tmap codecKey codecValue) (Map key value) where
  reflectCodec _ = Common.map (reflectCodec _codecKey) (reflectCodec _codecValue)
    where
    _codecKey = Proxy :: Proxy codecKey
    _codecValue = Proxy :: Proxy codecValue

-- | Same as `Data.Codec.Argonaut.Common.maybe`, where `Nothing`s are encoded
-- | with tags, making this version safe for nested values (e.g `Just (Just Nothing)`).
foreign import data Tmaybe :: TJsonCodec -> TJsonCodec
instance reflectCodecTmaybeMaybe :: IsJsonCodec codec a => IsJsonCodec (Tmaybe codec) (Maybe a) where
  reflectCodec _ = Common.maybe (reflectCodec _codec)
    where
    _codec = Proxy :: Proxy codec

-- | Same as `Data.Codec.Argonaut.Compat.maybe`, where `Nothing`s are encoded
-- | as `null`, making this version unsafe for nested values
-- | (e.g `Just (Just Nothing)`).
foreign import data TmaybeNull :: TJsonCodec -> TJsonCodec
instance reflectCodecTmaybeNullMaybe :: IsJsonCodec codec a => IsJsonCodec (TmaybeNull codec) (Maybe a) where
  reflectCodec _ = Compat.maybe (reflectCodec _codec)
    where
    _codec = Proxy :: Proxy codec

-- | Same as `Data.Codec.Argonaut.Common.tuple`
foreign import data Ttuple :: TJsonCodec -> TJsonCodec -> TJsonCodec
instance reflectCodecTtupleTuple :: (IsJsonCodec codecA a, IsJsonCodec codecB b) => IsJsonCodec (Ttuple codecA codecB) (Tuple a b) where
  reflectCodec _ = Common.tuple (reflectCodec _codecA) (reflectCodec _codecB)
    where
    _codecA = Proxy :: Proxy codecA
    _codecB = Proxy :: Proxy codecB

-- | Same as `Data.Codec.Argonaut.Compat.maybe`, where `Nothing`s are encoded
-- | as `null`, making this version unsafe for nested values
-- | (e.g `Just (Just Nothing)`).
foreign import data TnullarySum :: Symbol -> Type -> TJsonCodec
instance reflectCodecTnullarySum :: (IsSymbol type_name, Generic type_ r, NullarySumCodec r, IsJsonCodec codec type_) => IsJsonCodec (TnullarySum type_name type_) type_ where
  reflectCodec _ = CGeneric.nullarySum (reflectSymbol _type_name)
    where
    _type_name = Proxy :: Proxy type_name

-- | Works like `Data.Codec.Argonaut.Record.object`, the most convenient
-- | way to encode/decode a record
-- |
-- | ```
-- | type RecordTypeAndItsCodec =
-- |   Using
-- |     { foo :: String, bar :: Boolean }
-- |     (Tobject "MyRecord" ( foo :: Tstring, bar :: Tboolean ))
-- | ```
-- | ... or ...
-- | ```
-- | type RecordTypeAndItsCodec =
-- |   { foo :: String, bar :: Boolean } ::: (Tobject "MyRecord"
-- |   ( foo :: Tstring, bar :: Tboolean ))
-- | ```
foreign import data Tobject :: Symbol -> Row TJsonCodec -> TJsonCodec
instance reflectCodecTobjectRecord :: (
  RL.RowToList ri rl,
  TypeRowListCodec rl ri ro,
  IsSymbol sym
  ) => IsJsonCodec (Tobject sym ri) (Record ro) where
  reflectCodec _ = Codec.object (reflectSymbol _sym) (reflectRowListCodec _rl)
    where
    _sym = Proxy :: Proxy sym
    _rl = Proxy :: Proxy rl

foreign import data Tvariant :: Row TJsonCodec -> TJsonCodec
instance reflectCodecTvariantVariant :: (
  RL.RowToList ri rl,
  TypeVariantCodec rl ri ro
  ) => IsJsonCodec (Tvariant ri) (Variant ro) where
  reflectCodec _ = reflectVariantCodec _rl
    where
    _rl = Proxy :: Proxy rl

-- | Same as `Data.Codec.Argonaut.Migration.renameField`
foreign import data TrenameField :: Symbol -> Symbol -> TJsonCodec -> TJsonCodec
instance reflectCodecTrenameFieldJsonCodec :: (IsSymbol from, IsSymbol to, IsJsonCodec codec a) => IsJsonCodec (TrenameField from to codec) a where
  reflectCodec _ = CMigrate.renameField (reflectSymbol _from) (reflectSymbol _to) >~> (reflectCodec _codec)
    where
    _from = Proxy :: Proxy from
    _to = Proxy :: Proxy to
    _codec = Proxy :: Proxy codec

-- | Type class needed to implement `Tobject`'s `IsJsonCodec`.
class TypeRowListCodec :: RL.RowList TJsonCodec -> Row TJsonCodec -> Row Type -> Constraint
class TypeRowListCodec rl ri ro | rl -> ri ro where
  reflectRowListCodec :: Proxy rl -> JPropCodec (Record ro)

instance typeRowListCodecNil :: TypeRowListCodec RL.Nil () () where
  reflectRowListCodec _ = Codec.record

instance typeRowListCodecCons :: (
  TypeRowListCodec rest ri' ro',
  IsSymbol sym,
  Row.Cons sym codec ri' ri,
  IsJsonCodec codec a,
  Row.Cons sym a ro' ro
  ) => TypeRowListCodec (RL.Cons sym codec rest) ri ro where
  reflectRowListCodec _ =
    Codec.recordProp _sym (reflectCodec _codec) (reflectRowListCodec _rest)
    where
    _sym = Proxy :: Proxy sym
    _codec = Proxy :: Proxy codec
    _rest = Proxy :: Proxy rest

-- | Type class needed to implement `Tvariants`'s `IsJsonCodec`.
class TypeVariantCodec :: RL.RowList TJsonCodec -> Row TJsonCodec -> Row Type -> Constraint
class TypeVariantCodec rl ri ro | rl -> ri ro where
  reflectVariantCodec :: Proxy rl -> JsonCodec (Variant ro)

instance typeVariantCodecNil :: TypeVariantCodec RL.Nil () () where
  reflectVariantCodec _ = CVariant.variant

instance typeVariantCodecCons :: (
  TypeVariantCodec rest ri' ro',
  IsSymbol sym,
  Row.Cons sym codec ri' ri,
  IsJsonCodec codec a,
  Row.Cons sym a ro' ro
  ) => TypeVariantCodec (RL.Cons sym codec rest) ri ro where
  reflectVariantCodec _ =
    CVariant.variantCase _sym (Right (reflectCodec _codec)) (reflectVariantCodec _rest)
    where
    _sym = SProxy :: SProxy sym
    _codec = Proxy :: Proxy codec
    _rest = Proxy :: Proxy rest
