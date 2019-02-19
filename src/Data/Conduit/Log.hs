module Data.Conduit.Log where

import Prelude (Maybe(..), FilePath, Show)
import Prelude (show, appendFile, writeFile, return)
import Prelude (($), (++))
import Conduit (ConduitT, MonadIO)
import Conduit (await, liftIO, yield)

logC :: (MonadIO m, Show a) => FilePath -> ConduitT a a m ()
logC filename = do
    liftIO $ writeFile filename ""
    go filename
    where
        go f = do
            maybeItem <-  await
            case maybeItem of
                Nothing -> return ()
                Just item -> do
                    liftIO $ appendFile filename ((show item) ++ "\n")
                    yield item
                    go f
            