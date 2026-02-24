{-# LANGUAGE DeriveGeneric #-}
module Types.RequestTypes (Lab1InputData(..)) where
import Data.Aeson (FromJSON)
import Types.MathTypes
import GHC.Generics (Generic)

data Lab1InputData = Lab1InputData 
    { _n :: Int
    , _matrix :: Matrix
    , _vector :: Vector
    , _eps :: Double
    } deriving (Show, Generic)

instance FromJSON Lab1InputData
