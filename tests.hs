

average :: (Fractional a) => [a] -> a
average xs = (sum xs) / fromIntegral (length xs)


zeroCrossings :: (Num a, Ord a) => [a] -> Integer
zeroCrossings [] = 0 
zeroCrossings [x] = 0 
zeroCrossings (x:y:xs) = n + zeroCrossings (y:xs) where n = if (x > 0 && y <= 0) || (x < 0 && y >= 0) then 1 else 0 


extend ::  Num a => [a] -> [a]
extend [] = cycle [0]
extend [x] = repeat x
extend (x:xs) = x : extend xs



