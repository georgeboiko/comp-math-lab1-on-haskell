{-# LANGUAGE DeriveGeneric #-}
module Types.RequestTypes (Lab1InputData(..), Lab1GenerateData(..), Lab2InputChordData(..)) where
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

data Lab2InputChordData = Lab2InputChordData 
    { equationChordId :: Int
    , chordA :: Double
    , chordB :: Double
    , chordEps :: Double
    } deriving (Show, Generic)

instance FromJSON Lab2InputChordData