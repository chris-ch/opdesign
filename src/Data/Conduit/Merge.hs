module Data.Conduit.Merge where

import Prelude (Monad, Bool, Maybe(..))
import Prelude (otherwise)
import Control.Monad.State (lift)
import Prelude (($))
import Conduit (ConduitT)
import qualified Data.Conduit.Internal as CI

-- | Takes two monotonic sources and merges them.
-- This comes from https://github.com/luispedro/conduit-algorithms made available thanks to Luis Pedro Coelho.
mergeC2 :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () a m ()
mergeC2 comparator (CI.ConduitT s1) (CI.ConduitT s2) = CI.ConduitT $  processMergeC2 comparator s1 s2

-- newtype SealedConduitT i o m r = SealedConduitT (Pipe i i o () m r) 
-- newtype ConduitT i o m r = ConduitT { runConduitT :: forall b. (r -> Pipe i i o () m b) -> Pipe i i o () m b
processMergeC2 :: Monad m => (a -> a -> Bool)
                        -> ((() -> CI.Pipe () () a () m ()) -> CI.Pipe () () a () m ()) -- s1    ConduitT () a m ()
                        -> ((() -> CI.Pipe () () a () m ()) -> CI.Pipe () () a () m ()) -- s2    ConduitT () a m ()
                        -> ((() -> CI.Pipe () () a () m b ) -> CI.Pipe () () a () m b ) -- rest  ConduitT () a m ()
processMergeC2 comparator s1 s2 rest = go (s1 CI.Done) (s2 CI.Done)
    where
        go s1''@(CI.HaveOutput s1' v1) s2''@(CI.HaveOutput s2' v2)  -- s1''@ and s2''@ simply name the pattern expressions
            | comparator v1 v2 = CI.HaveOutput (go s1' s2'') v1
            | otherwise = CI.HaveOutput (go s1'' s2') v2
        go s1'@CI.Done{} (CI.HaveOutput s v) = CI.HaveOutput (go s1' s) v
        go (CI.HaveOutput s v) s1'@CI.Done{}  = CI.HaveOutput (go s s1')  v
        go CI.Done{} CI.Done{} = rest ()
        go (CI.PipeM p) left = do
            next <- lift p
            go next left
        go right (CI.PipeM p) = do
            next <- lift p
            go right next
        go (CI.NeedInput _ next) left = go (next ()) left
        go right (CI.NeedInput _ next) = go right (next ())
        go (CI.Leftover next ()) left = go next left
        go right (CI.Leftover next ()) = go right next

-- | Takes two monotonic sources and merges them.
--
-- zipC2 :: (Monad m) => (Maybe a -> Maybe a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () (Maybe a, Maybe a) m ()
-- zipC2 comparator (CI.ConduitT s1) (CI.ConduitT s2) = CI.ConduitT $ processZipC2 comparator s1 s2
-- processZipC2 :: Monad m => (Maybe a -> Maybe a -> Bool)
--                         -> ((() -> CI.Pipe () () (Maybe a) () m ()) -> CI.Pipe () () (Maybe a) () m ()) -- s1    ConduitT () a m ()
--                         -> ((() -> CI.Pipe () () (Maybe a) () m ()) -> CI.Pipe () () (Maybe a) () m ()) -- s2    ConduitT () a m ()
--                         -> ((() -> CI.Pipe () () (Maybe a, Maybe a) () m b ) -> CI.Pipe () () (Maybe a, Maybe a) () m b ) -- rest  ConduitT () a m ()
-- processZipC2 comparator s1 s2 rest = go (s1 CI.Done) (s2 CI.Done)
--     where
--         go s1''@(CI.HaveOutput s1' v1) s2''@(CI.HaveOutput s2' v2)  -- s1''@ and s2''@ simply name the pattern expressions
--             | comparator v1 v2 = CI.HaveOutput (go s1' s2'') (Nothing, Nothing)
--             | otherwise = CI.HaveOutput (go s1'' s2') (Nothing, Nothing)
--         go s1'@CI.Done{} (CI.HaveOutput s v) = CI.HaveOutput (go s1' s) v
--         go (CI.HaveOutput s v) s1'@CI.Done{}  = CI.HaveOutput (go s s1') v
--         go CI.Done{} CI.Done{} = rest ()
--         go (CI.PipeM p) left = do
--             next <- lift p
--             go next left
--         go right (CI.PipeM p) = do
--             next <- lift p
--             go right next
--         go (CI.NeedInput _ next) left = go (next ()) left
--         go right (CI.NeedInput _ next) = go right (next ())
--         go (CI.Leftover next ()) left = go next left
--         go right (CI.Leftover next ()) = go right next
