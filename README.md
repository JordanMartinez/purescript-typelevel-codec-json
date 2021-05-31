# purescript-typelevel-codec-json

## Why this library exists

Codecs convert values in the computer's memory to and from a format that can be sent elsewhere (e.g. JSON, XML, CSV, Binary, etc.). Because they are necessary but boring boilerplate, many developers will try to delegate this work to a tool or the compiler. One common way many developers do this is via typeclasses. For example, `purescript-argonaut-codecs`'s [`EncodeJson`](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/docs/Data.Argonaut.Encode.Class#t:EncodeJson) and [`DecodeJson`](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/docs/Data.Argonaut.Decode.Class#t:DecodeJson) and `purescript-simple-json`'s [`WriteForeign`](https://pursuit.purescript.org/packages/purescript-simple-json/docs/Simple.JSON#t:WriteForeign) and [`ReadForeign`](https://pursuit.purescript.org/packages/purescript-simple-json/docs/Simple.JSON#t:ReadForeign) are all typeclass-based codecs.

However convenient these might be, typeclass-based codecs have their own issues as described in [Some thoughts on typeclass-based codecs](https://code.slipthrough.net/2018/03/13/thoughts-on-typeclass-codecs/), namely:
- they are not good at migrating from one format to another (since a given type can only be encoded/decoded in one way)
- silent and unnoticeable changes to the codec can be easily made, especially when `Generic` is being used, which can break applications unexpectedly.

Thus, a library for value-based codecs called [`purescript-codec`](https://pursuit.purescript.org/packages/purescript-codec/) and its JSON-specific version, [`purescript-codec-argonaut`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/) were written. The value-based approach enables one to slowly migrate from one format to the next and ensures changes are noticed.

Unfortunately, you as a developer may still be forced to use the typeclass-based codecs for at least one valid reason:
- a library that uses a lot of type-level programming to ensure correctness (e.g. [`purescript-payload`](https://github.com/hoodunit/purescript-payload)) may force you to use a typeclass-based codec.

This library, `purescript-typelevel-codec-argonaut`, enables developers to use typeclass-based codecs as though they were value-level codecs.

Rather than writing something like this where we have no control over `purescript-argonaut-codecs` and `purescript-simple-json`'s decisions as to how to encode/decode these values:
```purescript
newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive newtype instance encodeJsonEmail :: EncodeJson Email
derive newtype instance decodeJsonEmail :: DecodeJson Email
derive newtype instance writeForeignEmail :: WriteForeign Email
derive newtype instance readForeignEmail :: ReadForeign Email

type MyRecord =
  { foo :: Email
  , bar :: Maybe (Maybe Int)
  , baz :: Array (Either Boolean (Tuple String Int))
  }

toArgonautJson :: MyRecord -> Json
toArgonautJson = encodeJson

toForeign :: MyRecord -> Foreign
toForeign = writeImpl
```

We can instead use the `Using` type to indicate which codec from `purescript-codec-argonaut` to use when encoding/decoding that value. For example
```purescript
-- defined in this library
newtype Using :: Type -> TJsonCodec -> Type
newtype Using a codec = Using a

-- defined in your code
newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _

type MyRecord =
  { foo :: Email
  , bar :: Maybe (Maybe Int)
  , baz :: Array (Either Boolean (Tuple String Int))
  }

-- Note: below we use '()' (i.e. row syntax), not '{}' (i.e. record syntax)
type MyRecordCodec = Tobject "MyRecord"
  ( foo :: Tnewtype Email Tstring
  , bar :: Tmaybe (Tmaybe Tint)
  , baz :: Tarray (Teither Tboolean (Ttuple Tstring Tint))
  )

toArgonautJson :: MyRecord -> Json
toArgonautJson r = encodeJson u
  where
  u :: Using MyRecord MyRecordCodec
  u = Using r

toForeign :: MyRecord -> Foreign
toForeign r = writeImpl u
  where
  u :: Using MyRecord MyRecordCodec
  u = Using r
```

## How to use this library

1. Familiarize yourself with [`purescript-codec-argonaut`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/)
    - a function named `foo` in that library will be `Tfoo` here.
1. See the [tests](./test/Test/Main.purs) for examples.
1. Read about the caveats to this library in the [`Type.Codec.Argonaut` module's documentation](./src/Type/Codec/Argonaut.purs)
