{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.SignalProcessing where

import Prelude (Int, Monad, Num, fromInteger, ($), (<*>), (<$>), (-), (+), (>>))
import Conduit (ConduitT)
import Conduit (yield, mapC, slidingWindowC)
import Conduit ((.|))
import qualified Conduit as DC (ZipSource(..), getZipSource)

shift :: (Monad m, Num a) => Int -> (ConduitT () a m ()) -> (ConduitT () a m ())
shift count signal = yield (fromInteger 0) >> signal .| slidingWindowC (count + 1) .| mapC delta
    where
        delta :: (Num a) => [a] -> a
        delta (m:n:_) = n - m 

operator :: (Monad m) => (a -> a -> a) -> (ConduitT () a m ()) -> (ConduitT () a m ()) -> (ConduitT () a m ())
operator func signal1 signal2 = DC.getZipSource $ func <$> DC.ZipSource signal1 <*> DC.ZipSource signal2
