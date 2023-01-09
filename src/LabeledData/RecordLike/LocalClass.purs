module LabeledData.RecordLike.LocalClass where

class RecordLike :: Type -> Type -> Row Type -> Constraint
class RecordLike tok a r | tok a -> r where
  toRecord :: tok -> a -> Record r
  fromRecord :: tok -> Record r -> a
