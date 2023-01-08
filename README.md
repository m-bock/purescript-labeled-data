# purescript-labeled-data


```hs
data Foo
  = MakeA Int                    String                 (Maybe Int)
  | MakeB Boolean
  | MakeC
  | MakeD { x :: Int, y :: Int }
  | MakeE Char                   { x :: Int, y :: Int }
```

It's a type called `Foo` which has 5 constructors:
  - constructor `MakeA` has two positional arguments (`Int`, `String`, `Maybe Int`)
  - constructor `MakeB` has one positional argument
  - constructor `MakeC` has no arguments
  - constructor `MakeD` has one argument (a record of shape `{ x :: Int, y :: Int }`)
  - constructor `MakeE` has two positional arguments (`Char`, a record of shape `{ x :: Int, y :: Int }`)

```hs
foo1 :: Foo
foo1 = MakeA 3 "bla" (Just 1)

foo2 :: Foo
foo2 = MakeC

foo3 :: Foo
foo3 = MakeE 'a' {x: 3, y: 2}
```

```hs
f :: Foo -> String
f foo = case foo of
  MakeA _ str _ -> str
  MakeB _       -> "B"
  MakeC         -> "C"
  MakeD _       -> "D"
  MakeE _ _     -> "E" 
```

```hs
type Vec = { x :: Int, y :: Int }
```
```
type FooV = Variant
  ( makeA :: Record ( _1 :: Int     , _2 :: String , _3 :: Boolean )
  , makeB :: Record ( _1 :: Boolean                                )
  , makeC :: Record (                                              )
  , makeD :: Record ( _1 :: Vec                                    )
  , makeE :: Record ( _1 :: Char    , _2 :: Vec                    )
  )
```

```hs
type FooV' = Variant
  ( makeA :: Record ( _1 :: Int                    , _3 :: Boolean )
  , makeC :: Record (                                              )
  , makeD :: Record ( _1 :: Vec                                    )
  )
```

```
foo1V :: FooV
foo1V = toVariant foo1
```