{-# LANGUAGE DeriveGeneric #-}
module Types.RequestTypes (Lab1InputData(..), Lab1GenerateData(..),
    Lab2InputEquationData(..), Lab2InputSystemData(..),
    Lab3InputIntegralData(..),
    Lab4InputData(..), Lab5InputData(..),
    Lab6InputData(..)) where
import Data.Aeson (FromJSON)
import Types.MathTypes
import GHC.Generics (Generic)

data Lab1InputData = Lab1InputData 
    { lab1N :: Int
    , lab1Matrix :: Matrix
    , lab1Vector :: Vector
    , lab1Eps :: Double
    } deriving (Show, Generic)

instance FromJSON Lab1InputData

data Lab1GenerateData = Lab1GenerateData 
    { lab1GenN :: Int
    , lab1GenEps :: Double
    } deriving (Show, Generic)

instance FromJSON Lab1GenerateData

data Lab2InputEquationData = Lab2InputEquationData 
    { lab2EquationId :: Int
    , lab2A :: Double
    , lab2B :: Double
    , lab2Eps :: Double
    } deriving (Show, Generic)

instance FromJSON Lab2InputEquationData

data Lab2InputSystemData = Lab2InputSystemData 
    { lab2SystemId :: Int
    , lab2InitialGuess :: Vector
    , lab2SystemEps :: Double
    } deriving (Show, Generic)


instance FromJSON Lab2InputSystemData

data Lab3InputIntegralData = Lab3InputIntegralData
    { lab3FunctionId :: Int
    , lab3A :: Double
    , lab3B :: Double
    , lab3Eps :: Double
    } deriving (Show, Generic)

instance FromJSON Lab3InputIntegralData

data Lab4InputData = Lab4InputData
    { lab4Points :: [(Double, Double)]
    } deriving (Show, Generic)

instance FromJSON Lab4InputData

data Lab5InputData = Lab5InputData
    { lab5Points :: [(Double, Double)]
    , lab5X      :: Double
    } deriving (Show, Generic)

instance FromJSON Lab5InputData

data Lab6InputData = Lab6InputData
    { lab6EquationId :: Int
    , lab6X0         :: Double
    , lab6Y0         :: Double
    , lab6Xn         :: Double
    , lab6H          :: Double
    , lab6Eps        :: Double
    } deriving (Show, Generic)

instance FromJSON Lab6InputData