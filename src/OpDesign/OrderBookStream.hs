{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.OrderBookStream where

import qualified Data.Conduit.Text as CText (lines)

import Data.List (dropWhileEnd)
import Data.Text (pack, unpack)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)

import Conduit ((.|))
import Conduit (ConduitT, ResourceT)
import Conduit (mapC, decodeUtf8C, scanlC)

import OpDesign.OrderBook (OrderBook, emptyOrderBook, updateOrderBook, fromTickData, tickFields)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

tickStream :: ConduitT ByteString String (ResourceT IO) ()
tickStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix

accumulate :: Monad m => ConduitT OrderBook OrderBook m ()
accumulate = scanlC updateOrderBook ( emptyOrderBook (read "2000-01-01 00:00:00" :: UTCTime))

orderBookStream :: Monad m => ConduitT String OrderBook m ()
orderBookStream = mapC tickFields .| mapC fromTickData .| accumulate
