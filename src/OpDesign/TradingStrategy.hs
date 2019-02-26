module OpDesign.TradingStrategy where

import Prelude (Eq, Show, Monad, Rational, Integer, Double, Maybe(..))
import Prelude (($), (&&), (>=), (<), (>), (==), (+), (-), (++), (*))
import Prelude (return, otherwise, show, fromIntegral)

import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time (UTCTime(..))

import Numeric (fromRat, showFFloat)

import Control.Monad.State (get, put, lift)
import Control.Monad.Trans.State.Strict (StateT)

import Conduit (await, yield, evalStateC)
import Conduit (ConduitT)

import OpDesign.OrderBook (OrderBook(..), Price, fromPrice)
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

type StateScalpingStrategy = (SinglePortfolioPosition, Maybe LimitOrder)

scalpingStrategyC :: (Monad m) => ConduitT OrderBook (LimitOrder, SinglePortfolioPosition) (StateT StateScalpingStrategy m) ()
scalpingStrategyC = do
        input <- await :: (Monad m) => ConduitT OrderBook (LimitOrder, SinglePortfolioPosition) (StateT StateScalpingStrategy m) (Maybe OrderBook)
        case input of
            Nothing -> return ()
            Just orderBook -> do
                (portfolioPosition, maybeLastOrder) <- lift get
                let maybeOrder = evaluateStrategy portfolioPosition orderBook maybeLastOrder
                case maybeOrder of

                    Just order -> do
                        lift $ put (updatePosition portfolioPosition order, Just order)
                        yield (order, updatePosition portfolioPosition order)

                    Nothing -> do
                        lift $ put (portfolioPosition, maybeLastOrder)
                        return ()

                scalpingStrategyC

        where
            evaluateStrategy portfolioPosition orderBook maybeLastOrder = case orderBook of
                OrderBook {bidVolume = Just _, bidPrice = Just bp, askPrice = Just ap, askVolume = Just _} -> case quantity portfolioPosition of
                    -- No position
                    0   | extractTime (date orderBook) >= (TimeOfDay 12 30 0) && extractTime (date orderBook) < (TimeOfDay 20 30 0) -> 
                            -- Enters position
                            Just $ buyOrder (date orderBook) 1 ap
                        ------
                        | otherwise -> Nothing
                        ------
                    -- In position
                    _   | extractTime (date orderBook) >= (TimeOfDay 20 30 0) ->
                        -- Liquidate
                        Just $ sellOrder (date orderBook) 1 bp
                        ------
                        | otherwise ->  case maybeLastOrder of
                            -- Tries to liquidate at a profit
                            Just lastOrder  -> if (fromPrice bp) > (price lastOrder) then Just $ sellOrder (date orderBook) 1 bp else Nothing
                            Nothing         -> Nothing
                        ------
                -- Invalid order book
                _ -> Nothing

sellOrder :: UTCTime -> Integer -> Price -> LimitOrder
sellOrder ts vol px = LimitOrder {
    timestamp = ts,
    volume = vol,
    price = fromPrice px,
    direction = Sell
}

buyOrder :: UTCTime -> Integer -> Price -> LimitOrder
buyOrder ts vol px = LimitOrder {
    timestamp = ts,
    volume = vol,
    price = fromPrice px,
    direction = Buy
}

scalpingStrategy :: (Monad m) => ConduitT OrderBook (LimitOrder, SinglePortfolioPosition) m ()
scalpingStrategy = evalStateC (SinglePortfolioPosition {quantity = 0}, Nothing) $ scalpingStrategyC

legProfit :: (Monad m) => ConduitT (LimitOrder, SinglePortfolioPosition) (UTCTime, Double) m ()
legProfit = do
    input1 <- await
    case input1 of
        Nothing -> return ()
        Just (lo1, sp1) -> do
            input2 <- await
            case input2 of
                Nothing -> return ()
                Just (lo2, sp2) -> do
                    let pnl = (fromIntegral (quantity sp2 - quantity sp1)) * (price lo1 - price lo2)
                    yield (timestamp lo2, (fromRat pnl :: Double))
                    legProfit
