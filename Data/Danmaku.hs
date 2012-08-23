{-# LANGUAGE Rank2Types #-}
module Data.Danmaku (
    -- * Basic Danmaku
    DanmakuT,
    Firing(..),
    execDanmakuT, 
    fire,
    tick,
    wait,
    parallelDanmaku,
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine
import Data.Maybe

type DanmakuT b m a = Coroutine (Firing b) m a

data Firing b c = Fire b c | Tick c

instance Functor (Firing x) where
    fmap f (Fire x y) = Fire x (f y)
    fmap f (Tick y) = Tick (f y)

instance Functor s => MonadTrans (Coroutine s) where -- 再定義しないとなぜか動かない
   lift = Coroutine . liftM Right

execDanmakuT :: Monad m => DanmakuT b m a -> m (Maybe ([b], DanmakuT b m a))
execDanmakuT danmaku = do
    v <- resume danmaku
    case v of
        Left (Fire x cont) -> do
            t <- execDanmakuT cont
            return $ case t of
                Just (xs, cont) -> Just (x : xs, cont)
                Nothing -> Nothing
        Left (Tick cont) -> return $ Just ([], cont)
        _ -> return $ Nothing

fire :: Monad m => b -> DanmakuT b m ()
fire x = suspend $ Fire x (return ())

tick :: Monad m => DanmakuT b m ()
tick = suspend $ Tick (return ())

wait :: Monad m => Int -> DanmakuT b m ()
wait = flip replicateM_ tick

parallelDanmaku :: Monad m => [DanmakuT b m ()] -> DanmakuT b m ()
parallelDanmaku xs = do
    (bss, ds) <- lift $ liftM unzip $ liftM catMaybes $ mapM execDanmakuT xs
    mapM_ fire $ concat bss
    unless (null ds) $ tick >> parallelDanmaku ds
