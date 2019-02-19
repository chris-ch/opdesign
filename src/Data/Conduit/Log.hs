module Data.Conduit.Log where

import Prelude (Maybe(..), IO, FilePath, Show)
import Prelude (show, appendFile, return)
import Prelude (($), (++))
import Conduit (ConduitT, MonadIO)
import Conduit (await, liftIO, yield)

logC :: (MonadIO m, Show a) => FilePath -> ConduitT a a m ()
logC filename = do
    maybeItem <-  await
    case maybeItem of
        Nothing -> return ()
        Just item -> do
            liftIO (appendFile filename ((show item) ++ "\n"))
            yield item
            logC filename
            