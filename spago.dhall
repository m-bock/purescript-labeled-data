{ name = "labeled-data"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "prelude"
  , "record"
  , "spec"
  , "tuples"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
