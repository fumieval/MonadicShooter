module Data.Bullet.Actual (
runBulletT,
ActualBulletT,
ActualBullet,
Yield(..),
yield,
PosAndArg,
boundBy,
uniformBullet,
linearBullet,
bulletWithVelocity
) where
import Data.Vect
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity
import Data.Bullet
import Debug.Trace

type PosAndArg = (Vec2, Float)

type ActualBulletT = BulletT PosAndArg

type ActualBullet = ActualBulletT Identity

boundBy :: Monad m => (Vec2 -> Bool) -> ActualBulletT m () -> ActualBulletT m ()
boundBy p b = do
    x <- lift $ runBulletT b
    case x of
        Left (Yield (pos, a) cont) | p pos -> yield (pos, a) >> boundBy p cont
        _ -> return ()

boundSecondary :: Monad m => (Vec2 -> Bool) -> ActualBulletT m () -> ActualBulletT m ()
boundSecondary p b = do
    x <- lift $ runBulletT b
    case x of
        Left (Yield (pos, a) cont)
            | p pos -> yield (pos, a) >> boundBy p cont
            | otherwise -> boundSecondary p cont
        _ -> return ()
    
uniformBullet :: Monad m => Float -- angle
    -> Float -- speed
    -> Vec2 -- initial position
    -> ActualBulletT m a
uniformBullet a s pos = yield (pos, a) >> uniformBullet a s (pos &+ sinCos a &* s)

linearBullet :: Monad m => Float -> Vec2
    -> BulletT Float m a
    -> ActualBulletT m a
linearBullet a pos = bulletWithVelocity pos . mapSuspension sMap
    where
        sMap (Yield s x) = Yield (sinCos a &* s) x

bulletWithVelocity :: Monad m => Vec2
    -> BulletT Vec2 m a
    -> ActualBulletT m a
bulletWithVelocity pos b = do
    r <- lift $ resume b
    case r of
        Left (Yield v cont) -> do
            let pos' = pos &+ v
            yield (pos', angle2 v)
            bulletWithVelocity pos' cont
        Right a -> return a
        