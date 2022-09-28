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
countSteps content = if stepSensor (getContent content) then "Step!" else content 


getContent :: String -> [Double]
getContent input = 
    let datapoints = map read (lines input) :: [(Double, Double, Double)]
        summedData = map (\(a, b, c) -> a + b + c) datapoints
        dataLength = length summedData
        processedData =
            applyFilter (hpf highPassCutoff) $
                applyFilter (lpf lowPassCutoff) $
                    reverse summedData
                    in summedData


stepSensor :: [Double] -> String
stepSensor [] = []
stepSensor [_] = []
stepSensor (x:y:ys) 
    |x == y = (x:y:ys)
    |x > 0 && y <= 0 = "Step" 
    | otherwise (x:y:ys)  stepSensor (y:ys)
