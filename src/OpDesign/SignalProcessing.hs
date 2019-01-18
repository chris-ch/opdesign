{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.SignalProcessing where

import Prelude (IO, Int, Monad, Num, Double, pi, round, map, fromIntegral, fromInteger, sin, ($), (*), (<*>), (<$>), (-), (+), (/), (>>))
import Conduit (ConduitT, Identity)
import Conduit (yield, yieldMany, mapC, slidingWindowC)
import Conduit ((.|))
import qualified Conduit as DC (ZipSource(..), getZipSource)

type Signal a = ConduitT () a Identity ()

shift :: (Num a) => Int -> Signal a -> Signal a
shift count signal = yield (fromInteger 0) >> signal .| slidingWindowC (count + 1) .| mapC delta
    where
        delta :: (Num a) => [a] -> a
        delta (m:n:_) = n - m 

operator :: (a -> a -> a) -> Signal a -> Signal a -> Signal a
operator func signal1 signal2 = DC.getZipSource $ func <$> DC.ZipSource signal1 <*> DC.ZipSource signal2

-- period is measured in number of samples
sinusoidal :: Int -> Int -> Signal Int
sinusoidal period amplitude = yieldMany [round ( (fromIntegral amplitude) * sin (scale period n) ) | n  <- [0..]]
    where
        scale :: Int -> Int -> Double
        scale m n = 2 * pi * fromIntegral n / fromIntegral m
