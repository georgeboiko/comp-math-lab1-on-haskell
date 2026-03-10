{-# LANGUAGE DeriveGeneric #-}
module Types.ResponseTypes (Response(..),
    Lab1Response(..), Lab1OutputData(..),
    Lab2Response(..), Lab2OutputData(..)) where
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

data Lab2OutputData = Lab2OutputData
    { lab2IsSuccess :: Bool
    , lab2EquationString :: String
    , lab2EquationLatex :: String
    , lab2Root :: Double
    , lab2Value :: Double
    , lab2ErrMessage :: String
    , lab2Iters :: Int
    } deriving (Show, Generic)

instance ToJSON Lab2OutputData

data Lab2Response = Lab2Response
    { lab2Info :: Response
    , lab2Payload :: Lab2OutputData
    } deriving (Show, Generic)

instance ToJSON Lab2Response