{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.SignalProcessing where

import Prelude (IO, Int, Monad, Num, Double)
import Prelude (replicate, pi, round, cycle, map, fromIntegral, fromInteger, sin)
import Prelude (($), (*), (++), (<*>), (<$>), (-), (+), (/), (>>))
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
genSinusoid :: Int -> Int -> Signal Int
genSinusoid period amplitude = yieldMany [round ( (fromIntegral amplitude) * sin (scale period n) ) | n  <- [0..]]
    where
        scale :: Int -> Int -> Double
        scale m n = 2 * pi * fromIntegral n / fromIntegral m

-- delay is measured in number of samples
genStep :: Int -> Signal Int
genStep delay = yieldMany $ (replicate delay 0) ++ cycle [1]

-- countZero: counts how many samples are at zero
-- countOne: counts how many samples ar at one
genSquare :: Int -> Int -> Signal Int
genSquare countZero countOne = yieldMany $ cycle $ (replicate countZero 0) ++ (replicate countOne 1)

genConstant :: (Num a) => a -> Signal a
genConstant k = yieldMany $ cycle [k]

opNegate :: (Num a) => Signal a -> Signal a
opNegate = operator (-) (genConstant 0)

opAdd :: (Num a) => Signal a -> Signal a -> Signal a
opAdd input1 input2 = operator (+) input1 input2

opSub :: (Num a) => Signal a -> Signal a -> Signal a
opSub input1 input2 = operator (-) input1 input2

opMul :: (Num a) => Signal a -> Signal a -> Signal a
opMul input1 input2 = operator (*) input1 input2
