module Data.Conduit.Log where

import Prelude (Maybe(..), FilePath, Show, IO)
import Prelude (show, return)
import Prelude (($), (++))
import System.IO (hPutStrLn, hClose, openFile, IOMode(..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Conduit (ConduitT, MonadIO)
import Conduit (await, liftIO, yield)

logC :: (MonadIO m, Show a) => FilePath -> ConduitT a a m ()
logC filename = do
    handle <- liftIO $ openFile filename WriteMode
    go handle
    where
        go h = do
            maybeItem <-  await
            case maybeItem of
                Nothing -> do
                    liftIO $ hClose h
                    return ()
                Just item -> do
                    timestamp <- liftIO (getCurrentTime :: IO UTCTime)
                    liftIO $ hPutStrLn h (show timestamp ++ " - " ++ show item)
                    yield item
                    go h
            