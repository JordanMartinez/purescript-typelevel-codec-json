{ name = "typelevel-codec-argonaut"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "bifunctors"
  , "codec"
  , "codec-argonaut"
  , "effect"
  , "either"
  , "foreign"
  , "foreign-object"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
