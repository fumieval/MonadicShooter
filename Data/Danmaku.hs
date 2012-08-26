
module Data.Danmaku (
    -- * Basic Danmaku
    DanmakuT,
    Firing(..),
    evolveDanmakuT,
    fire,
    tick,
    wait,
    parallelDanmaku,
    embedDanmakuState,
) where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Coroutine
import Data.Maybe
import Data.Bullet

type DanmakuT b m = BulletT (Firing b) m

data Firing b c = Fire b c | Tick c

instance Functor (Firing x) where
    fmap f (Fire x y) = Fire x (f y)
    fmap f (Tick y) = Tick (f y)

evolveDanmakuT :: Monad m => DanmakuT b m a -> m (Maybe (DanmakuT b m a), [b])
evolveDanmakuT danmaku = do
    v <- resume danmaku
    case v of
        Left (Fire x cont) -> do
            (cont', xs) <- evolveDanmakuT cont
            return (cont', x:xs)
        Left (Tick cont) -> return (Just cont, [])
        _ -> return (Nothing, [])

fire :: Monad m => b -> DanmakuT b m ()
fire x = suspend $ Fire x (return ())

tick :: Monad m => DanmakuT b m ()
tick = suspend $ Tick (return ())

wait :: Monad m => Int -> DanmakuT b m ()
wait = flip replicateM_ tick

embedDanmakuState :: Monad m
    => s
    -> DanmakuT b (StateT s m) ()
    -> DanmakuT b m ()
embedDanmakuState initial bullet = do
    ((cont, xs), s) <- lift $ evolveDanmakuT bullet `runStateT` initial
    mapM_ fire xs
    tick
    maybe (return ()) (embedDanmakuState s) cont

parallelDanmaku :: Monad m => [DanmakuT b m ()] -> DanmakuT b m ()
parallelDanmaku xs = do
    (ds, bss) <- lift $ liftM unzip $ mapM evolveDanmakuT xs
    mapM_ fire $ concat bss
    let ds' = catMaybes ds
    unless (null ds') $ tick >> parallelDanmaku ds'