{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OpDesign.OrderBookStream where

import qualified Data.Conduit.Text as CText (lines)

import Data.List (dropWhileEnd)
import Data.Text (pack, unpack)
import Data.ByteString (ByteString)

import Conduit ((.|))
import Conduit (ConduitM, ResourceT)
import Conduit (mapC, decodeUtf8C)

import OpDesign.OrderBook (OrderBook, emptyOrderBook, updateOrderBook, fromTickData, tickFields)

-- drops possible '\r' endings
dos2unix :: String -> String
dos2unix = dropWhileEnd (== '\r')

tickStream :: ConduitM ByteString OrderBook (ResourceT IO) ()
tickStream = decodeUtf8C
                .| CText.lines
                .| mapC unpack
                .| mapC dos2unix
                .| mapC tickFields
                .| mapC fromTickData

orderBookStream :: ConduitM ByteString OrderBook (ResourceT IO) ()
orderBookStream = tickStream .| mapC (updateOrderBook emptyOrderBook)
