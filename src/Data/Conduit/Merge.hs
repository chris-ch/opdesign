module Data.Conduit.Merge where

import Prelude (Monad, Bool(..), Maybe(..))
import Prelude (return, const)
import Prelude ((>>))
import Conduit (ConduitT)
import Conduit (takeWhileC, dropC, peekC, yield)
import Conduit ((.|))

-- | Takes two sources and merges them.
-- This comes from https://github.com/luispedro/conduit-algorithms made available thanks to Luis Pedro Coelho.
mergeC2 :: (Monad m) => (a -> a -> Bool) -> ConduitT () a m () -> ConduitT () a m () -> ConduitT () a m ()
mergeC2 comparator c1 c2 = do
    ma <- c1 .| peekC
    mb <- c2 .| peekC
    case (ma, mb) of
        (Just a, Just b) -> case comparator a b of 
            True -> do
                yield a
                mergeC2 comparator (c1 .| drop1) c2
            False -> do
                yield b
                mergeC2 comparator c1 (c2 .| drop1)

        (Just a, Nothing) -> do
            yield a
            mergeC2 comparator (c1 .| drop1) c2

        (Nothing, Just b) -> do
            yield b
            mergeC2 comparator c1 (c2 .| drop1)

        _ -> return ()

    where
        drop1 = dropC 1 >> takeWhileC (const True)

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
