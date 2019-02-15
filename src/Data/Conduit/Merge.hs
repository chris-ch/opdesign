module Data.Conduit.Merge where

import Prelude (Monad, Bool, Maybe(..), Show, Eq)
import Prelude (otherwise, return)
import Prelude (($))
import Conduit (ConduitT)
import Conduit (evalStateC, mapC, yield, await)
import Conduit ((.|))
import Control.Monad.State (get, put, lift)
import Control.Monad.Trans.State.Strict (StateT)

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

data MergeTag = LeftItem | RightItem deriving (Show, Eq)
data TaggedItem a = TaggedItem MergeTag a deriving (Show, Eq)
mergeTag :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () (TaggedItem a) m ()
mergeTag func series1 series2 = mergeC2 (tagSort func) taggedSeries1 taggedSeries2
                where
                    taggedSeries1 = series1 .| mapC (\item -> TaggedItem LeftItem item)
                    taggedSeries2 = series2 .| mapC (\item -> TaggedItem RightItem item)
                    tagSort :: (a -> a -> Bool) -> TaggedItem a -> TaggedItem a -> Bool
                    tagSort f (TaggedItem _ item1) (TaggedItem _ item2) = f item1 item2

type StateMergePair a = (Maybe a, Maybe a)
pairTagC :: (Monad m) => ConduitT  (TaggedItem a) (StateMergePair a) (StateT (StateMergePair a) m) ()
pairTagC = do
    input <- await
    case input of
        Nothing -> return ()
        Just taggedItem -> do
            stateMergePair <- lift get
            let outputState = updateStateMergePair taggedItem stateMergePair
            lift $ put outputState
            yield outputState
            pairTagC

updateStateMergePair :: TaggedItem a -> StateMergePair a -> StateMergePair a
updateStateMergePair (TaggedItem tag item) (Just leftItem, Just rightItem) = case tag of
    LeftItem -> (Just item, Just rightItem)
    RightItem -> (Just leftItem, Just item)

updateStateMergePair (TaggedItem tag item) (Nothing, Just rightItem) = case tag of
    LeftItem -> (Just item, Just rightItem)
    RightItem -> (Nothing, Just item)

updateStateMergePair (TaggedItem tag item) (Just leftItem, Nothing) = case tag of
    LeftItem -> (Just item, Nothing)
    RightItem -> (Just leftItem, Just item)

updateStateMergePair (TaggedItem tag item) (Nothing, Nothing) = case tag of
    LeftItem -> (Just item, Nothing)
    RightItem -> (Nothing, Just item)

pairTag :: (Monad m) => ConduitT (TaggedItem a) (StateMergePair a) m ()
pairTag = evalStateC (Nothing, Nothing) pairTagC

mergeSort :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () (StateMergePair a) m ()
mergeSort func series1 series2 = mergeTag func series1 series2 .| pairTag


keepJustC :: (Monad m) => ConduitT (StateMergePair a) (a, a) m ()
keepJustC = do
    input <- await
    case input of
        Nothing -> return ()
        Just mergePair -> do
            case mergePair of
                (Just item1, Just item2) -> yield (item1, item2)
                _ -> return ()
                
            keepJustC
