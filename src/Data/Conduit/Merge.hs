module Data.Conduit.Merge where

import Prelude (Monad, Bool(..), Maybe(..))
import Prelude (otherwise, return, const)
import Prelude (($), (>>))
import Conduit (ConduitT)
import Conduit (takeWhileC, dropC, peekC, yield)
import Conduit ((.|))
import Control.Monad.State (lift)

import qualified Data.Conduit.Internal as CI

-- | Takes two sources and merges them.
-- This comes from https://github.com/luispedro/conduit-algorithms made available thanks to Luis Pedro Coelho.
mergeC2 :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () a m ()
mergeC2 comparator (CI.ConduitT s1) (CI.ConduitT s2) = CI.ConduitT $  processMergeC2 comparator s1 s2

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


zipUpdateDef :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> (Maybe a, Maybe a) -> ConduitT () (Maybe a, Maybe a) m ()
zipUpdateDef f c1 c2 (s1, s2) = do
    ma <- c1 .| peekC
    mb <- c2 .| peekC
    case (ma, mb) of
        (Just a, Just b) ->
            case (f a b, f b a) of
                (True, True) -> do
                    yield (ma, mb)
                    zipUpdateDef f (c1 .| drop1) (c2 .| drop1) (ma, mb)
                (_, True) -> do
                    yield (s1, mb)
                    zipUpdateDef f c1 (c2 .| drop1) (s1, mb)
                (True, _) -> do
                    yield (ma, s2)
                    zipUpdateDef f (c1 .| drop1) c2 (ma, s2)
                _ ->
                    zipUpdateDef f (c1 .| drop1) (c2 .| drop1) (ma, s2)
        (Just _, Nothing) -> do
            yield (ma, s2)
            zipUpdateDef f (c1 .| drop1) c2 (ma, s2)
        (Nothing, Just _) -> do
            yield (s1, mb)
            zipUpdateDef f c1 (c2 .| drop1) (s1, mb)
        _ -> return ()
  where
    drop1 = dropC 1 >> takeWhileC (const True)

zipUpdate :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () (Maybe a, Maybe a) m ()
zipUpdate f c1 c2 = zipUpdateDef f c1 c2 (Nothing, Nothing)
