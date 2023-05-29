module LabeledData.RecordLike.Class where

import Prelude

import Data.Tuple (Tuple)
import LabeledData.RecordLike.Generic (class GenericRecordLike, genericFromRecord, genericToRecord)
import LabeledData.TransformEntry.Transforms (NoTransform)
import Type.Proxy (Proxy(..))

class RecordLike a r | a -> r where
  toRecord :: a -> Record r
  fromRecord :: Record r -> a

instance (GenericRecordLike NoTransform (Tuple a b) r) => RecordLike (Tuple a b) r where
  toRecord = genericToRecord (Proxy :: _ NoTransform)
  fromRecord = genericFromRecord (Proxy :: _ NoTransform)

instance RecordLike (Record r) r where
  toRecord = identity
  fromRecord = identity
