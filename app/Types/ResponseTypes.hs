{-# LANGUAGE DeriveGeneric #-}
module Types.ResponseTypes (Response(..), Lab1Response(..), Lab1OutputData(..)) where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Types.MathTypes

data Response = Response 
    { resStatus  :: String
    , resCode    :: Int
    , resMessage :: String 
    } deriving (Show, Generic)

instance ToJSON Response

data Lab1OutputData = Lab1OutputData
    { lab1IsSuccess :: Bool
    , lab1ClientMatrix :: Matrix
    , lab1ClientVector :: Vector
    , lab1AnsVector :: Vector
    , lab1ErrVector :: Vector
    , lab1Iters :: Int
    , lab1Norm :: Double
    } deriving (Show, Generic)

instance ToJSON Lab1OutputData

data Lab1Response = Lab1Response
    { lab1Info :: Response
    , lab1Payload :: Lab1OutputData
    } deriving (Show, Generic)

instance ToJSON Lab1Response