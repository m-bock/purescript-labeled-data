module LabeledData.RecordLike.Class where

import Prelude

import LabeledData.RecordLike.Generic (class GenericRecordLike, genericFromRecord, genericToRecord)
import Data.Tuple (Tuple)

class RecordLike a r | a -> r where
  toRecord :: a -> Record r
  fromRecord :: Record r -> a

instance (GenericRecordLike (Tuple a b) r) => RecordLike (Tuple a b) r where
  toRecord = genericToRecord
  fromRecord = genericFromRecord

instance RecordLike (Record r) r where
  toRecord = identity
  fromRecord = identity
