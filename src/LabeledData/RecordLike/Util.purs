module LabeledData.RecordLike.Util where

toSingelton :: forall a. a -> { _1 :: a }
toSingelton _1 = { _1 }

fromSingelton :: forall a. { _1 :: a } -> a
fromSingelton { _1 } = _1