module Data.Bullet.Actual (
runBulletT,
ActualBulletT,
ActualBullet,
Yield(..),
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

type PosAndArg = (Vec2, Float)

type ActualBulletT = BulletT (Yield PosAndArg)

type ActualBullet = ActualBulletT Identity

boundBy :: Monad m => (Vec2 -> Bool) -> ActualBulletT m () -> ActualBulletT m ()
boundBy p b = do
    x <- lift $ runBulletT b
    case x of
        Just (Yield (pos, _) cont) | p pos -> boundBy p cont
        _ -> return ()

uniformBullet :: Monad m => Float -- angle
    -> Float -- speed
    -> Vec2 -- initial position
    -> ActualBulletT m a
uniformBullet a s pos = yield (pos, a) >> uniformBullet a s (pos &+ sinCos a &* s)

linearBullet :: Monad m => Float -> Vec2
    -> BulletT (Yield Float) m a
    -> ActualBulletT m a
linearBullet a pos = bulletWithVelocity pos . mapSuspension sMap
    where
        sMap (Yield s x) = Yield (sinCos a &* s) x

bulletWithVelocity :: Monad m => Vec2
    -> BulletT (Yield Vec2) m a
    -> ActualBulletT m a
bulletWithVelocity pos b = do
    r <- lift $ resume b
    case r of
        Left (Yield v cont) -> do
            let pos' = pos &+ v
            yield (pos', angle2 v)
            bulletWithVelocity pos' cont
        Right a -> return a
        