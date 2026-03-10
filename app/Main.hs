{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Types.RequestTypes (Lab1InputData(..), Lab1GenerateData(..), Lab2InputEquationData(..), Lab2InputSystemData(..))
import Types.ResponseTypes
import Processors.Lab1SimpleIterationsProcessor
import Processors.Lab2ChordProcessor
import Processors.Lab2NewtonProcessor
import Processors.Lab2SimpleIterationsProcessor
import Processors.Lab2SystemNewtonProcessor
import Utils.Generators

main :: IO ()
main = scotty 8000 $ do

    post "/api/lab/1" $ do
        requestData <-jsonData :: ActionM Lab1InputData
        payload <- liftIO $ processLab1Data requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab1Response {lab1Info = info, lab1Payload = payload}
        json response

    post "/api/lab/1/generate" $ do
        requestData <-jsonData :: ActionM Lab1GenerateData

        matrix <- liftIO $ generateMatrix (lab1GenN requestData) (lab1GenN requestData)
        vector <- liftIO $ generateVector (lab1GenN requestData)
        let inputData = Lab1InputData {
              lab1N = lab1GenN requestData
            , lab1Matrix = matrix
            , lab1Vector = vector
            , lab1Eps = lab1GenEps requestData 
            }
        payload <- liftIO $ processLab1Data inputData

        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab1Response {lab1Info = info, lab1Payload = payload}
        json response
    
    post "/api/lab/2/chord" $ do
        requestData <- jsonData :: ActionM Lab2InputEquationData
        payload <- liftIO $ processLab2ChordData requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2Response {lab2Info = info, lab2Payload = payload}
        json response
    
    post "/api/lab/2/newton" $ do
        requestData <- jsonData :: ActionM Lab2InputEquationData
        payload <- liftIO $ processLab2NewtonData requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2Response {lab2Info = info, lab2Payload = payload}
        json response

    post "/api/lab/2/iters" $ do
        requestData <- jsonData :: ActionM Lab2InputEquationData
        payload <- liftIO $ processLab2SimpleIterationsData requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2Response {lab2Info = info, lab2Payload = payload}
        json response

    post "/api/lab/2/system" $ do
        requestData <- jsonData :: ActionM Lab2InputSystemData
        payload <- liftIO $ processLab2NewtonSystemData requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2SystemResponse {lab2SystemInfo = info, lab2SystemPayload = payload}
        json response

    get "/api/health/check" $ do
        json $ Response "OK" 200 "Server is healthy"