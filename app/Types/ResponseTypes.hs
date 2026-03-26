{-# LANGUAGE DeriveGeneric #-}
module Types.ResponseTypes (Response(..),
    Lab1Response(..), Lab1OutputData(..),
    Lab2Response(..), Lab2OutputData(..),
    Lab2OutputSystemData(..), Lab2SystemResponse(..),
    Lab2EquationData(..), Lab2SystemData(..),
    Lab3OutputData(..), Lab3Response(..)) where
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

data Lab2OutputSystemData = Lab2OutputSystemData
    { lab2SystemIsSuccess :: Bool
    , lab2SystemEquationString :: [String]
    , lab2SystemEquationLatex :: String
    , lab2SystemRoot :: Vector
    , lab2SystemErrMessage :: String
    , lab2SystemErrVector :: Vector
    , lab2SystemIters :: Int
    } deriving (Show, Generic)

instance ToJSON Lab2OutputSystemData

data Lab2Response = Lab2Response
    { lab2Info :: Response
    , lab2Payload :: Lab2OutputData
    } deriving (Show, Generic)

instance ToJSON Lab2Response

data Lab2SystemResponse = Lab2SystemResponse
    { lab2SystemInfo :: Response
    , lab2SystemPayload :: Lab2OutputSystemData
    } deriving (Show, Generic)

instance ToJSON Lab2SystemResponse

data Lab2EquationData = Lab2EquationData
    { equationId     :: Int
    , equationString :: String
    , equationLatex  :: String
    } deriving (Show, Generic)

instance ToJSON Lab2EquationData

data Lab2SystemData = Lab2SystemData
    { systemInfoId      :: Int
    , systemInfoStrings :: [String]
    , systemInfoLatex   :: String
    } deriving (Show, Generic)

instance ToJSON Lab2SystemData

data Lab3OutputData = Lab3OutputData
    { lab3IsSuccess :: Bool
    , lab3IntegralString :: String
    , lab3IntegralLatex :: String
    , lab3CalculatedIntegral :: Double
    , lab3ErrValue :: Double
    , lab3PartsCount :: Int
    , lab3Message :: String
    } deriving (Show, Generic)

instance ToJSON Lab3OutputData

data Lab3Response = Lab3Response
    { lab3Info :: Response
    , lab3Payload :: Lab3OutputData
    } deriving (Show, Generic)

instance ToJSON Lab3Response