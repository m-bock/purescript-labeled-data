{ name = "labeled-data"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "maybe"
  , "prelude"
  , "record"
  , "tuples"
  , "type-equality"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
