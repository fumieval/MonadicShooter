module Data.Danmaku where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Coroutine
import Data.Maybe

type DanmakuT i b m = Coroutine (Firing b) (ReaderT i m) ()

data Firing b c = Fire b c | Tick c

instance Functor (Firing x) where
    fmap f (Fire x y) = Fire x (f y)
    fmap f (Tick y) = Tick (f y)

instance Functor s => MonadTrans (Coroutine s) where -- 再定義しないとなぜか動かない
   lift = Coroutine . liftM Right

runDanmaku :: Monad m => i -> DanmakuT i b m -> m (Maybe ([b], DanmakuT i b m))
runDanmaku input danmaku = do
    v <- runReaderT (resume danmaku) input
    case v of
        Left (Fire x cont) -> do
            t <- runDanmaku input cont
            return $ case t of
                Just (xs, cont) -> Just (x : xs, cont)
                Nothing -> Nothing
        Left (Tick cont) -> return $ Just ([], cont)
        _ -> return $ Nothing

fire :: Monad m => b -> DanmakuT i b m
fire x = suspend $ Fire x (return ())

tick :: Monad m => DanmakuT i b m
tick = suspend $ Tick (return ())

wait :: Monad m => Int -> DanmakuT i b m
wait = flip replicateM_ tick

observe :: Monad m => Coroutine (Firing b) (ReaderT i m) i
observe = lift ask

parallelDanmaku :: Monad m => [DanmakuT i b m] -> DanmakuT i b m
parallelDanmaku xs = do
    input <- observe
    (bss, ds) <- lift $ lift $ liftM unzip $ liftM catMaybes $ mapM (runDanmaku input) xs
    return ()
    mapM_ fire $ concat bss
    unless (null ds) $ tick >> parallelDanmaku ds