{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.Text hiding (intercalate)
import Data.List
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

-- GRowToList from kcsongor
type family GRowToList (r :: * -> *) :: [(Symbol, *)] where
  GRowToList (l :*: r)
    = GRowToList l ++ GRowToList r
  GRowToList (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a))
    = '[ '(name, a) ]
  GRowToList (M1 _ m a)
    = GRowToList a
  GRowToList U1 = '[]

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- define some representations of data that can be purescript
class HasPSRep r where
  toPSRep :: Proxy r -> String

instance HasPSRep Int where
  toPSRep _ = "Int"

-- sigh...
instance HasPSRep Text where
  toPSRep _ = "String"

instance
  ( HasPSRep a
  ) => HasPSRep [a] where
  toPSRep _ = "List (" ++ toPSRep (Proxy @a) ++ ")"

-- what if we wanted to work with records?
-- this ain't no purescript, so we have to work with concrete product types :'(
-- but luckily, we can make a generic function, and use our GRowToList
class RecordFields (fields :: [(Symbol, *)]) where
  getRecordFields :: Proxy fields -> [(String, String)]

instance RecordFields '[] where
  getRecordFields _ = mempty

instance
  ( KnownSymbol name
  , HasPSRep ty
  , RecordFields tail
  ) => RecordFields ('(name, ty) ': tail) where
  getRecordFields _ = pair : rest
    where
      pair = (symbolVal $ Proxy @name, toPSRep $ Proxy @ty)
      rest = getRecordFields (Proxy @tail)

-- now we can define a generic function for records
-- i kind of choose to throw away the constructor here
genericRecordToPSRep :: forall a fields
   . Generic a
  => fields ~ (GRowToList (Rep a))
  => RecordFields fields
  => Proxy a
  -> String
genericRecordToPSRep _ = "\n  { " ++ fields ++ "\n  }"
  where
    fields = intercalate "\n  , " $ pairs
    pairs = format <$> getRecordFields (Proxy @fields)
    format (name, ty) = name ++ " :: " ++ ty

-- so i can now use this with my data type
data MyRecord = MyRecord
  { a :: Text
  , b :: Int
  , c :: [Text]
  } deriving (Generic)

instance HasPSRep MyRecord where
  toPSRep = genericRecordToPSRep
