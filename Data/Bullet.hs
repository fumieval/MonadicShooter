module Data.Bullet where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Arrow
import Data.Functor.Identity
import Data.Vect

type ImageName = String

type BulletT = Coroutine

instance Functor s => MonadTrans (Coroutine s) where -- 再定義しないとなぜか動かない
   lift = Coroutine . liftM Right

runBulletT :: (Monad m, Functor f) => BulletT f m a -> m (Maybe (f (BulletT f m a)))
runBulletT = liftM (either Just (const Nothing)) . resume


data ActualBulletAttributes = ActualBulletAttributes
    {
        bulletPosition :: Vec2
        ,bulletArgument :: Float
        ,bulletImage :: String
        ,bulletRadius :: Float
    }

    
type ActualBullet = BulletT (Yield ActualBulletAttributes)

constantLinearBullet :: Monad m => Float -- speed
    -> ActualBulletAttributes -> ActualBullet m a
constantLinearBullet s attr = linearBullet attr $ forever $ yield s

linearBullet :: Monad m => ActualBulletAttributes
    -> BulletT (Yield Float) m a
    -> ActualBullet m a
linearBullet attrs = bulletWithVelocity attrs . mapSuspension sMap
    where
        sMap (Yield s x) = Yield (sinCos (bulletArgument attrs) &* s) x

bulletWithVelocity :: Monad m => ActualBulletAttributes
    -> BulletT (Yield Vec2) m a
    -> ActualBullet m a
bulletWithVelocity attrs b = do
    r <- lift $ resume b
    case r of
        Left (Yield v cont) -> do
            let attrs' = attrs { bulletPosition = bulletPosition attrs &+ v
                               , bulletArgument = angle2 v
                               }
            yield attrs'
            bulletWithVelocity attrs' cont
        Right a -> return a
        