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
    bulletWithVelocity,
    bulletWithVelocity'
) where
import Data.Vect.Double
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Bullet
import Data.Functor.Identity

type PosAndArg = (Vec2, Double)

type ActualBulletT = BulletT PosAndArg

type ActualBullet = ActualBulletT Identity

boundBy :: Monad m => (Vec2 -> Bool) -> ActualBulletT m () -> ActualBulletT m ()
boundBy p bullet = bound bullet where
    bound b = do
        x <- lift $ runBulletT b
        case x of
            Left (Yield (pos, a) cont) | p pos -> yield (pos, a) >> bound cont
            _ -> return ()

boundSecondary :: Monad m => (Vec2 -> Bool) -> ActualBulletT m () -> ActualBulletT m ()
boundSecondary p b = do
    x <- lift $ runBulletT b
    case x of
        Left (Yield (pos, a) cont)
            | p pos -> yield (pos, a) >> boundBy p cont
            | otherwise -> boundSecondary p cont
        _ -> return ()
    
uniformBullet :: Monad m => Double -- angle
    -> Double -- speed
    -> Vec2 -- initial position
    -> ActualBulletT m a
uniformBullet a s pos = bullet pos where
    bullet pos = yield (pos, a) >> bullet (pos &+ sinCos a &* s)

linearBullet :: Monad m => Double -> Vec2
    -> BulletT Double m a
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

bulletWithVelocity' :: Monad m => Vec2
    -> BulletT Vec2 (ReaderT Vec2 m) a
    -> ActualBulletT m a
bulletWithVelocity' pos b = do
    r <- lift $ resume b `runReaderT` pos
    case r of
        Left (Yield v cont) -> do
            let pos' = pos &+ v
            yield (pos', angle2 v)
            bulletWithVelocity' pos' cont
        Right a -> return a
