# fun-purescript-type-generation-from-haskell

A fun way to generate PureScript types from Haskell using a RowList approximation. Totally not a complete kosher way to do this, but at least demonstrates the idea.

## tl;dr

```hs
data MyRecord = MyRecord
  { a :: Text
  , b :: Int
  , c :: [Text]
  } deriving (Generic)

instance HasPSRep MyRecord where
  toPSRep = genericRecordToPSRep

main = do
  let
    myRecordPSRep = toPSRep (Proxy @MyRecord)
    myType = "type MyRecord = " ++ myRecordPSRep
  putStrLn myType
```

```hs
type MyRecord = 
  {a :: String
  , b :: Int
  , c :: List (String)
  }
```
