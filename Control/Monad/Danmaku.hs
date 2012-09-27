{-# LANGUAGE BangPatterns #-}
module Control.Monad.Danmaku (
    -- * Danmaku definition
    DanmakuT,
    Firing(..),
    fire,
    tick,
    -- * Operations in Danmaku
    wait,    
    evolveDanmakuT,
    mapFire,
    -- * Transformation and Composition in Danmaku
    emptyDanmaku,
    parallelDanmaku,
    embedDanmakuState,
    -- * Danmaku with bullets
    embedToBullet,
    flatten,
) where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Bullet
import Data.Maybe
import Data.Either
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Debug.Trace

type DanmakuT b t m = Coroutine (Firing b t) m

data Firing b t c = Fire b c | Tick t c

instance Functor (Firing a b) where
    fmap f (Fire x y) = Fire x (f y)
    fmap f (Tick x y) = Tick x (f y)

-- | Put a thing.
fire :: Monad m => b -> DanmakuT b t m ()
fire x = suspend $ Fire x (return ())

-- | Proceed one step.
tick :: Monad m => t -> DanmakuT b t m ()
tick x = suspend $ Tick x (return ())

-- | Proceed given steps.
wait :: Monad m => Int -> t -> DanmakuT b t m ()
wait n t = replicateM_ n (tick t)

runDanmakuT :: Monad m => DanmakuT b t m a -> m (Either (Firing b t (DanmakuT b t m a)) a)
runDanmakuT = resume

-- | A danmaku that produces nothing forever.
emptyDanmaku :: Monad m => DanmakuT b () m a
emptyDanmaku = forever $ tick ()

-- | Compute a next state of danmaku and produced thing.
evolveDanmakuT :: Monad m => DanmakuT b t m a -> m (Either (Yield ([b], t) (DanmakuT b t m a)) a)
evolveDanmakuT danmaku = do
    r <- resume danmaku
    case r of
        Left (Fire x cont) -> do
            r <- evolveDanmakuT cont
            case r of
                Left (Yield (xs, t) cont') -> return $! Left (Yield (x : xs, t) cont')
                Right a -> return $! Right a
        Left (Tick x cont) -> return $! Left (Yield ([], x) cont)
        Right a -> return $! Right a

mapFire :: Monad m => (b -> Maybe b') -> DanmakuT b t m a -> DanmakuT b' t m a
mapFire f danmaku = do
    r <- lift $ resume danmaku
    case r of
        Left (Fire x cont) -> maybe (return ()) fire (f x) >> mapFire f cont
        Left (Tick x cont) -> tick x >> mapFire f cont
        Right a -> return a

mapTick :: Monad m => (t -> Maybe t') -> DanmakuT b t m a -> DanmakuT b t' m a
mapTick f danmaku = do
    r <- lift $ resume danmaku
    case r of
        Left (Fire x cont) -> fire x >> mapTick f cont
        Left (Tick x cont) -> maybe (return ()) tick (f x) >> mapTick f cont
        Right a -> return a
        
-- | Transform danmaku that has state to a mere danmaku. 
embedDanmakuState :: Monad m
    => s -- ^initial state
    -> DanmakuT b t (StateT s m) a -- ^original danmaku
    -> DanmakuT b t m a
embedDanmakuState initial danmaku = do
    (r, s) <- lift $ evolveDanmakuT danmaku `runStateT` initial
    case r of
        Left (Yield (bs, t) cont) -> mapM_ fire bs >> tick t >> embedDanmakuState s cont
        Right a -> return a

-- | Combine two danmakus into one and run in parallel.
parallelDanmaku :: Monad m => DanmakuT b t m a -> DanmakuT b u m b -> DanmakuT b (t, u) m (Either a b)
parallelDanmaku a b = do
    r <- lift $ evolveDanmakuT a
    case r of
        Left (Yield (bs, t) cont) -> do
            r <- lift $ evolveDanmakuT b
            case r of
                Left (Yield (cs, u) cont') -> do
                    mapM_ fire (bs ++ cs)
                    tick (t, u)
                    parallelDanmaku cont cont'
                Right b -> return $! Right b 
        Right a -> return $! Left a

-- | Convert a bullet into a bullet that produces something produced by the danmaku. 
embedToBullet :: Monad m => (s -> t -> u) -> BulletT s m a -> DanmakuT b t (ReaderT s m) c -> BulletT u (WriterT (Seq.Seq b) m) (Either a c)
embedToBullet f bullet danmaku = do
    r <- lift $ lift $ resume bullet
    case r of
        Left (Yield s bullet') -> do
            r <- lift $ lift $ evolveDanmakuT danmaku `runReaderT` s
            case r of
                Left (Yield (xs, t) cont) -> do
                    lift $ tell $ Seq.fromList xs
                    yield (f s t)
                    embedToBullet f bullet' cont
                Right c -> return $! Right c
        Right a -> return $! Left a

-- | Transform a danmaku that produces bullets producing something, into a danmaku that produces it.
flatten :: Monad m => DanmakuT (BulletT s (WriterT (Seq.Seq b) m) c) t m a -> DanmakuT (Either s b) t m a
flatten = flatten' [] where
    flatten' tamas danmaku = do
        tss <- forM tamas $ \t -> do
            (r, bs) <- lift $ runWriterT $ runBulletT t
            F.mapM_ (fire . Right) bs
            case r of
                Left (Yield s cont) -> fire (Left s) >> return [cont]
                Right _ -> return []
        r <- lift $ evolveDanmakuT danmaku
        case r of
            Left (Yield (xs, t) cont) -> tick t >> flatten' (xs ++ concat tss) cont
            Right a -> return a