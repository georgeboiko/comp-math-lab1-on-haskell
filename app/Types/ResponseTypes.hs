{-# LANGUAGE DeriveGeneric #-}
module Types.ResponseTypes (Response(..), Lab1Response(..), Lab1OutputData(..)) where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Types.MathTypes

data Response = Response 
    { _status  :: String
    , _code    :: Int
    , _message :: String 
    } deriving (Show, Generic)

instance ToJSON Response

data Lab1OutputData = Lab1OutputData
    { _isSuccess :: Bool
    , _ansVector :: Vector
    , _errVector :: Vector
    , _iters :: Int
    , _norm :: Double
    } deriving (Show, Generic)

instance ToJSON Lab1OutputData

data Lab1Response = Lab1Response
    { _info :: Response
    , _payload :: Lab1OutputData
    } deriving (Show, Generic)

instance ToJSON Lab1Response