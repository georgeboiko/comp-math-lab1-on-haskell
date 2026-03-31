{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Types.RequestTypes
import Types.ResponseTypes
import Processors.Lab1Processor
import Processors.Lab2Processor
import Processors.Lab3Processor
import Methods.Lab1SimpleIterationsMethod
import Methods.Lab2ChordMethod
import Methods.Lab2NewtonMethod
import Methods.Lab2SimpleIterationsMethod
import Methods.Lab2SystemNewtonMethod
import Methods.Lab3RectanglesMethod
import Methods.Lab3TrapezeMethod
import Methods.Lab3SimpsonMethod
import Methods.Lab3MonteCarloMethod
import Methods.Lab3RussianRouletteMethod
import Utils.Generators
import Utils.EquationStorage

main :: IO ()
main = scotty 8000 $ do

    post "/api/lab/1" $ do
        requestData <-jsonData :: ActionM Lab1InputData
        payload <- liftIO $ processLab1Data SystemSimpleItersMethod requestData
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
        payload <- liftIO $ processLab1Data SystemSimpleItersMethod inputData

        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab1Response {lab1Info = info, lab1Payload = payload}
        json response

    get "/api/lab/2/equations" $ do
        let response = zipWith (\i eq -> Lab2EquationData
                { equationId     = i
                , equationString = fString eq
                , equationLatex  = fLatex eq
                }) [0..] equations
        json response

    get "/api/lab/2/systems" $ do
        let response = zipWith (\i sys -> Lab2SystemData
                { systemInfoId      = i
                , systemInfoStrings = systemStrings sys
                , systemInfoLatex   = systemLatex sys
                }) [0..] systems
        json response
    
    post "/api/lab/2/chord" $ do
        requestData <- jsonData :: ActionM Lab2InputEquationData
        payload <- liftIO $ processLab2EquationData ChordMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2Response {lab2Info = info, lab2Payload = payload}
        json response
    
    post "/api/lab/2/newton" $ do
        requestData <- jsonData :: ActionM Lab2InputEquationData
        payload <- liftIO $ processLab2EquationData NewtonEqMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2Response {lab2Info = info, lab2Payload = payload}
        json response

    post "/api/lab/2/iters" $ do
        requestData <- jsonData :: ActionM Lab2InputEquationData
        payload <- liftIO $ processLab2EquationData EqSimpleItersMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2Response {lab2Info = info, lab2Payload = payload}
        json response

    post "/api/lab/2/system" $ do
        requestData <- jsonData :: ActionM Lab2InputSystemData
        payload <- liftIO $ processLab2SystemData NewtonSystemMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab2SystemResponse {lab2SystemInfo = info, lab2SystemPayload = payload}
        json response

    get "/api/lab/3/integrals" $ do
        let response = zipWith (\i func -> Lab3IntegralData
                { integralId     = i
                , integralString = functionString func
                , integralLatex  = functionLatex func
                }) [0..] functions
        json response

    post "/api/lab/3/rectangles/left" $ do
        requestData <- jsonData :: ActionM Lab3InputIntegralData
        payload <- liftIO $ processLab3IntegralData LeftRectanglesMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab3Response {lab3Info = info, lab3Payload = payload}
        json response

    post "/api/lab/3/rectangles/middle" $ do
        requestData <- jsonData :: ActionM Lab3InputIntegralData
        payload <- liftIO $ processLab3IntegralData MiddleRectanglesMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab3Response {lab3Info = info, lab3Payload = payload}
        json response

    post "/api/lab/3/rectangles/right" $ do
        requestData <- jsonData :: ActionM Lab3InputIntegralData
        payload <- liftIO $ processLab3IntegralData RightRectanglesMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab3Response {lab3Info = info, lab3Payload = payload}
        json response
    
    post "/api/lab/3/trapeze" $ do
        requestData <- jsonData :: ActionM Lab3InputIntegralData
        payload <- liftIO $ processLab3IntegralData TrapezeMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab3Response {lab3Info = info, lab3Payload = payload}
        json response
    
    post "/api/lab/3/simpson" $ do
        requestData <- jsonData :: ActionM Lab3InputIntegralData
        payload <- liftIO $ processLab3IntegralData SimpsonMethod requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab3Response {lab3Info = info, lab3Payload = payload}
        json response

    post "/api/lab/3/montecarlo" $ do
        requestData <- jsonData :: ActionM Lab3InputIntegralData
        let maxN = 1000000
        randomArray <- liftIO $ generateRandomValues maxN (lab3A requestData) (lab3B requestData)
        payload <- liftIO $ processLab3IntegralData (MonteCarloMethod randomArray) requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab3Response {lab3Info = info, lab3Payload = payload}
        json response

    post "/api/lab/3/russianroulette" $ do
        requestData <- jsonData :: ActionM Lab3InputIntegralData
        let maxN = 1000000
        valuesArray <- liftIO $ generateRandomValues maxN (lab3A requestData) (lab3B requestData)
        rouletteArray <- liftIO $ generateRandomValues maxN 0 1
        payload <- liftIO $ processLab3IntegralData (RussianRouletteMethod valuesArray rouletteArray) requestData
        let info = Response { resStatus = "OK", resCode = 200, resMessage = "Answer was calculated successfully" }
        let response = Lab3Response {lab3Info = info, lab3Payload = payload}
        json response

    get "/api/health/check" $ do
        json $ Response "OK" 200 "Server is healthy"