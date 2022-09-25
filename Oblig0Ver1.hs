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


countSteps content = if checkIfStep content then "Step" else content 

checkIfStep :: [a] -> Bool 
checkIfStep [] = False
checkIfStep xs 
            | datapoints = map xs :: [(Double, Double, Double)]
            | summedData = map (\(a, b, c) -> a + b + c) datapoints
            | dataLength = length summedData
            | processedData = applyFilter (hpf highPassCutoff) $ applyFilter (lpf lowPassCutoff) $ reverse summedData
            | stepCount = (`div` 2) $ zeroCrossings $ reverse $ take dataLength processedData
            where  stepCount == 1 



