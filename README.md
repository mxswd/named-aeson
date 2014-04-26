named-aeson
===========

Name json keys with type lits.

For example:

```hs
data TBanana = TBanana
  { tshape :: Field "banana-shape" Text
  , tsize  :: Field "banana size" (Maybe Int)
  , tname  :: Field "banana's name" Text
  } deriving Show

deriveToJSONFields ''TBanana

b = Banana "foo" (Just 2) "bar"
```

Will derive both a `TBanana` fielded data type and instance, plus a
`Banana` data type and instance without the `Field` newtype.

```hs
  deriveToJSONFields ''TBanana
======>
  examples/Banana.hs:12:1-28
  data Banana
    = Banana {shape :: Text, size :: Maybe Int, name :: Text}
  instance ToJSON TBanana where
    toJSON (TBanana a_a4eA a_a4eB a_a4eC)
      = object
          [(.=) "banana-shape" (unField a_a4eA),
           (.=) "banana size" (unField a_a4eB),
           (.=) "banana's name" (unField a_a4eC)]
  instance ToJSON Banana where
    toJSON (Banana a_a4eA a_a4eB a_a4eC)
      = object
          [(.=) "banana-shape" a_a4eA, (.=) "banana size" a_a4eB,
           (.=) "banana's name" a_a4eC]
```

