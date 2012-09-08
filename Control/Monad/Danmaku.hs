
module Control.Monad.Danmaku (
    -- * Danmaku definition
    DanmakuT,
    Firing(..),
    fire,
    tick,
    -- * Operations in Danmaku
    wait,    
    evolveDanmakuT,
    mapShot,
    -- * Transformation and Composition in Danmaku
    emptyDanmaku,
    parallelDanmaku,
    embedDanmakuState,
    -- * Danmaku with bullets
    embedToBullet,
    flatten,
    dispatch
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

type DanmakuT b m = Coroutine (Firing b) m

data Firing b c = Fire b c | Tick c

instance Functor (Firing x) where
    fmap f (Fire x y) = Fire x (f y)
    fmap f (Tick y) = Tick (f y)

-- | Put a thing.
fire :: Monad m => b -> DanmakuT b m ()
fire x = suspend $ Fire x (return ())

-- | Proceed one step.
tick :: Monad m => DanmakuT b m ()
tick = suspend $ Tick (return ())

-- | Proceed given steps.
wait :: Monad m => Int -> DanmakuT b m ()
wait = flip replicateM_ tick

runDanmakuT :: Monad m => DanmakuT b m a -> m (Either (Firing b (DanmakuT b m a)) a)
runDanmakuT = resume

-- | A danmaku that produces nothing forever.
emptyDanmaku :: Monad m => DanmakuT b m a
emptyDanmaku = forever tick

-- | Compute a next state of danmaku and produced thing.
evolveDanmakuT :: Monad m => DanmakuT b m a -> m (Either (DanmakuT b m a) a, [b])
evolveDanmakuT danmaku = do
    r <- resume danmaku
    case r of
        Left (Fire x cont) -> do
            (cont', xs) <- evolveDanmakuT cont
            return (cont', x:xs)
        Left (Tick cont) -> return (Left cont, [])
        Right a -> return (Right a, [])

mapShot :: Monad m => (b -> Maybe b') -> DanmakuT b m a -> DanmakuT b' m a
mapShot f danmaku = do
    r <- lift $ resume danmaku
    case r of
        Left (Fire x cont) -> maybe (return ()) fire (f x) >> mapShot f cont
        Left (Tick cont) -> tick >> mapShot f cont
        Right a -> return a
    
-- | Transform danmaku that has state to a mere danmaku. 
embedDanmakuState :: Monad m
    => s -- ^initial state
    -> DanmakuT b (StateT s m) () -- ^original danmaku
    -> DanmakuT b m ()
embedDanmakuState initial danmaku = do
    ((cont, xs), s) <- lift $ evolveDanmakuT danmaku `runStateT` initial
    mapM_ fire xs
    tick
    either (embedDanmakuState s) return cont

-- | Compose danmakus into one and run in parallel.
parallelDanmaku :: Monad m => [DanmakuT b m a] -> DanmakuT b m ()
parallelDanmaku xs = do
    (ds, bss) <- lift $ liftM unzip $ mapM evolveDanmakuT xs
    mapM_ fire $ concat bss
    let ds' = lefts ds
    unless (null ds') $ tick >> parallelDanmaku ds'

-- | Convert a bullet into a bullet that produces something produced by the danmaku. 
embedToBullet :: Monad m => BulletT s m a -> DanmakuT b (ReaderT s m) a -> BulletT s (WriterT (Seq.Seq b) m) a
embedToBullet bullet danmaku = do
    r <- lift $ lift $ resume bullet
    case r of
        Left (Yield s cont) -> do
            (danmaku', xs) <- lift $ lift $ evolveDanmakuT danmaku `runReaderT` s
            lift $ tell $ Seq.fromList xs
            yield s
            either (embedToBullet cont) return danmaku'
        Right a -> return a

-- | Transform a danmaku that produces bullets producing something, into a danmaku that produces it.
flatten :: Monad m => DanmakuT (BulletT s (WriterT (Seq.Seq b) m) c) m a -> DanmakuT (Either s b) m a
flatten = flatten' [] where
    flatten' tamas danmaku = do
        tss <- forM tamas $ \t -> do
            (r, bs) <- lift $ runWriterT $ runBulletT t
            F.mapM_ (fire . Right) bs
            case r of
                Left (Yield s cont) -> fire (Left s) >> return [cont]
                Right _ -> return []
        tick
        (cont, ts) <- lift $ evolveDanmakuT danmaku
        either (flatten' $ ts ++ concat tss) return cont

dispatch :: Monad m => DanmakuT (BulletT s (ReaderT r m) c) (WriterT (Last r) m) a
    -> DanmakuT (BulletT s m ()) m ()
dispatch = route' [] where
    route' tamas danmaku = do
        ((cont, ts), Last v') <- lift $ runWriterT $ evolveDanmakuT danmaku
        case v' of
            Nothing -> return ()
            Just v -> do
                tss <- forM tamas $ \t -> do
                    r <- lift $ runBulletT t `runReaderT` v
                    case r of
                        Left (Yield s cont) -> fire (yield s) >> return (Just cont)
                        Right _ -> return Nothing
                tick
                either (route' (ts ++ catMaybes tss)) (const $ return ()) cont
