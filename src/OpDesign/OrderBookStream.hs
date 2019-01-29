{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.OrderBookStream where

import qualified Data.Conduit.Text as CText (lines)

import Data.List (dropWhileEnd)
import Data.Text (pack, unpack)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)

import Conduit ((.|))
import Conduit (ConduitT, ResourceT, await)
import Conduit (mapC, decodeUtf8C, scanlC)

import OpDesign.OrderBook (OrderBook(..), updateOrderBook, fromTickData, tickFields, isValid, fromPrice)

scanl1C :: Monad m => (a -> a -> a) -> ConduitT a a m ()
scanl1C f = await >>= maybe (return ()) (scanlC f)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

tickStream :: ConduitT ByteString String (ResourceT IO) ()
tickStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix

orderBookStream :: Monad m => ConduitT String OrderBook m ()
orderBookStream = mapC tickFields .| mapC fromTickData .| scanl1C updateOrderBook

trfMidPrice :: Monad m => ConduitT OrderBook (Maybe Rational) m ()
trfMidPrice = mapC midPrice
    where
        midPrice :: OrderBook -> Maybe Rational
        midPrice OrderBook {date=_, bidVolume=Just _, bidPrice=Just bid, askPrice=Just ask, askVolume=Just _ } = Just ((fromPrice bid + fromPrice ask) / 2)
        midPrice _ = Nothing


