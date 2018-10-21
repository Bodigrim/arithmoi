module Main where

filterIsPrimeBench :: [(Double, Double, Double)]
filterIsPrimeBench =
  [ (1000, 1000, 912.7)
  , (10000, 1000, 1003)
  , (10000, 10000, 10160)
  , (100000, 1000, 1044)
  , (100000, 10000, 10700)
  , (100000, 100000, 111000)
  , (1000000, 1000, 1084)
  , (1000000, 10000, 11040)
  , (1000000, 100000, 101000)
  , (1000000, 1000000, 1119000)
  , (10000000, 1000, 1086)
  , (10000000, 10000, 14240)
  , (10000000, 100000, 97010)
  , (10000000, 1000000, 1164000)
  , (10000000, 10000000, 10510000)
  ]

filterIsPrimeRegression :: (Double, Double, Double)
filterIsPrimeRegression = (alpha, betaX, betaY)
  where
    avg xs = sum xs / fromIntegral (length xs)
    meanX = avg $ map (\(x, _, _) -> x) filterIsPrimeBench
    meanY = avg $ map (\(_, y, _) -> y) filterIsPrimeBench
    meanT = avg $ map (\(_, _, t) -> t) filterIsPrimeBench



main : IO ()
main = print ()
