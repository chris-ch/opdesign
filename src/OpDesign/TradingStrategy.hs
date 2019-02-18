module OpDesign.TradingStrategy where

import Prelude (Eq, Show, Monad, Rational, Integer, Double, Maybe(..))
import Prelude (($), (/=), (&&), (>=), (<), (==), (+), (-), (++))
import Prelude (return, otherwise, show)

import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time (UTCTime(..))

import Numeric (fromRat, showFFloat)

import Control.Monad.State (get, put, lift)
import Control.Monad.Trans.State.Strict (StateT)

import Conduit (await, yield, evalStateC)
import Conduit (ConduitT)

import OpDesign.OrderBook (OrderBook(..), fromPrice)
import OpDesign.OrderBookStream (extractTime)

data TradeDirection = Buy | Sell deriving (Eq, Show)

data LimitOrder = LimitOrder {
    timestamp :: UTCTime,
    volume :: Integer,
    price :: Rational,
    direction :: TradeDirection
} deriving (Eq)

instance Show LimitOrder where
    show (LimitOrder ts vol px dir) = "< LO: " ++ (show ts) ++ " - " ++ (show dir) ++ " " ++ (show vol) ++ " @ " ++ (toFloatStr (fromRat px :: Double)) "" ++ " >"
        where
            toFloatStr = showFFloat (Just 6)

data SinglePortfolioPosition = SinglePortfolioPosition {
    quantity :: Integer
} deriving (Eq, Show)

updatePosition :: SinglePortfolioPosition -> LimitOrder -> SinglePortfolioPosition
updatePosition portfolio order
    | direction order == Buy    = portfolio { quantity = quantity portfolio + volume order}
    | otherwise                 = portfolio { quantity = quantity portfolio - volume order}

type StateScalpingStrategy = SinglePortfolioPosition

scalpingStrategyC :: (Monad m) => ConduitT OrderBook (LimitOrder, SinglePortfolioPosition) (StateT StateScalpingStrategy m) ()
scalpingStrategyC = do
        input <- await :: (Monad m) => ConduitT OrderBook (LimitOrder, SinglePortfolioPosition) (StateT StateScalpingStrategy m) (Maybe OrderBook)
        case input of
            Nothing -> return ()
            Just orderBook -> do
                prevPortfolioPosition <- lift get :: (Monad m) => ConduitT OrderBook (LimitOrder, SinglePortfolioPosition) (StateT SinglePortfolioPosition m) SinglePortfolioPosition
                let maybeOrder = evaluateStrategy prevPortfolioPosition orderBook
                case maybeOrder of

                    Just order -> do
                        lift $ put (updatePosition prevPortfolioPosition order)
                        yield (order, updatePosition prevPortfolioPosition order)

                    Nothing -> do
                        lift $ put prevPortfolioPosition
                        return ()

                scalpingStrategyC

        where
            evaluateStrategy prevPortfolioPosition orderBook = case orderBook of
                OrderBook {bidVolume = Just _, bidPrice = Just bp, askPrice = Just ap, askVolume = Just _}
                    | quantity prevPortfolioPosition == 0 && extractTime (date orderBook) >= (TimeOfDay 20 45 0) && extractTime (date orderBook) < (TimeOfDay 20 50 0) -> 
                        Just $ LimitOrder {
                            timestamp = date orderBook, 
                            volume = 1, 
                            price = fromPrice ap, 
                            direction = Buy
                            }
                    | quantity prevPortfolioPosition /= 0 && extractTime (date orderBook) >= (TimeOfDay 20 50 0) ->
                        Just $ LimitOrder {
                            timestamp = date orderBook,
                            volume = 1,
                            price = fromPrice bp,
                            direction = Sell
                            }
                    | otherwise -> Nothing
                _ -> Nothing

scalpingStrategy :: (Monad m) => ConduitT OrderBook (LimitOrder, SinglePortfolioPosition) m ()
scalpingStrategy = evalStateC (SinglePortfolioPosition {quantity = 0}) $ scalpingStrategyC
