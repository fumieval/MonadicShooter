{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Bullet (BulletT, runBulletT, resume, suspend) where

import Control.Monad
import Control.Monad.Coroutine
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Arrow
import Data.Functor.Identity
import Data.Vect

type BulletT = Coroutine

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

runBulletT :: (Monad m, Functor f) => BulletT f m a -> m (Maybe (f (BulletT f m a)))
runBulletT = liftM (either Just (const Nothing)) . resume
