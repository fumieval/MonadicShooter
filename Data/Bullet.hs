{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Bullet (
    BulletT,
    runBulletT,
    yieldingToResult,
    takeBullet,
    embedBulletState) where

import Control.Monad
import Control.Monad.Coroutine
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Arrow
import Data.Functor.Identity
import Data.Vect

type BulletT y = Coroutine (Yield y)

instance Functor s => MonadTrans (Coroutine s) where -- 再定義しないとなぜか動かない
   lift = Coroutine . liftM Right

instance (Functor s, MonadReader r m) => MonadReader r (Coroutine s m) where
    ask = lift ask
    local f = Coroutine . local f . resume
    reader = lift . reader

instance (Functor f, MonadState s m) => MonadState s (Coroutine f m) where
    get = lift get
    put = lift . put
    state = lift . state

runBulletT :: Monad m => BulletT b m a -> m (Either (Yield b (BulletT b m a)) a)
runBulletT = resume

naughtBullet :: Monad m => BulletT () m ()
naughtBullet = forever $ yield ()

embedBulletState :: Monad m => s -> BulletT b (StateT s m) a -> BulletT b m a
embedBulletState initial bullet = do
    (r, s) <- lift $ resume bullet `runStateT` initial
    case r of
        Left (Yield x cont) -> yield x >> embedBulletState s cont
        Right a -> return a

yieldingToResult :: Monad m => a -> BulletT a m b -> BulletT a m a
yieldingToResult prev bullet = do
    r <- lift $ resume bullet
    case r of
        Left (Yield x cont) -> yield x >> yieldingToResult x cont
        Right _ -> return prev 
    
takeBullet :: Monad m => Int -> BulletT b m a -> BulletT b m ()
takeBullet 0 _ = return ()
takeBullet n bullet = do
    r <- lift $ resume bullet
    case r of
        Left (Yield x cont) -> yield x >> takeBullet (n - 1) cont
        Right _ -> return ()
