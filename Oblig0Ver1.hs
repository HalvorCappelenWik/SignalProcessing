module Main where

import Oblig0Common
    ( applyFilter,
        highPassCutoff,
    hpf,
    lowPassCutoff,
    lpf,
    zeroCrossings,
    )

import System.IO


main = interact countSteps 


countSteps :: String -> String 
countSteps content =  stepSensor (getContent (content))


getContent :: String -> [Double]
getContent input = 
    let datapoints = map read (lines input) :: [(Double, Double, Double)]
        summedData = map (\(a, b, c) -> a + b + c) datapoints
        dataLength = length summedData
        processedData =
            applyFilter (hpf highPassCutoff) $
                applyFilter (lpf lowPassCutoff) $
                    summedData
                    in summedData


stepSensor :: [Double] -> String
stepSensor [] = []
stepSensor [_] = []
stepSensor (x:y:ys) 
    | x == y = []
    | (x > 0 && y <= 0) =  "Step";  stepSensor (y:ys)
    | otherwise = stepSensor (y:ys) 