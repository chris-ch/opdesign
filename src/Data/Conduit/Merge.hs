module Data.Conduit.Merge where

import Prelude (Monad, Bool, Maybe)
import Prelude (otherwise)
import Control.Monad.State (lift)
import Prelude (($))
import Conduit (ConduitT)
import qualified Data.Conduit.Internal as CI

-- | Takes two monotonic sources and merges them.
--
mergeC2 :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () a m ()
mergeC2 comparator (CI.ConduitT s1) (CI.ConduitT s2) = CI.ConduitT $  processMergeC2 comparator s1 s2

processMergeC2 :: Monad m => (a -> a -> Bool)
                        -> ((r -> CI.Pipe () i1 a () m r) -> CI.Pipe () i2 a () m r) -- s1
                        -> ((r -> CI.Pipe () i3 a () m r) -> CI.Pipe () i4 a () m r) -- s2
                        -> (() -> CI.Pipe () i5 a () m r5) -- rest
                        -> CI.Pipe () i5 a () m r5
processMergeC2 comparator s1 s2 rest = let
        go right@(CI.HaveOutput s1' v1) left@(CI.HaveOutput s2' v2)
            | comparator v1 v2 = CI.HaveOutput (go s1' left) v1
            | otherwise = CI.HaveOutput (go right s2') v2
        go right@CI.Done{} (CI.HaveOutput s v) = CI.HaveOutput (go right s) v
        go (CI.HaveOutput s v) left@CI.Done{}  = CI.HaveOutput (go s left)  v
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
    in go (s1 CI.Done) (s2 CI.Done)

-- | Takes two monotonic sources and merges them.
--
-- zipC2 :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () (Maybe a, Maybe a) m ()
-- zipC2 comparator (CI.ConduitT s1) (CI.ConduitT s2) = CI.ConduitT $ processZipC2 comparator s1 s2

-- processZipC2 comparator s1 s2 rest = let
--         go right@(CI.HaveOutput s1' v1) left@(CI.HaveOutput s2' v2)
--             | comparator v1 v2 = CI.HaveOutput (go s1' left) v1
--             | otherwise = CI.HaveOutput (go right s2') v2
--         go right@CI.Done{} (CI.HaveOutput s v) = CI.HaveOutput (go right s) v
--         go (CI.HaveOutput s v) left@CI.Done{}  = CI.HaveOutput (go s left)  v
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
--     in go (s1 CI.Done) (s2 CI.Done)
