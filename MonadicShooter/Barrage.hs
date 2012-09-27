module MonadicShooter.Barrage (globalBarrage) where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Danmaku
import Control.Monad.Bullet
import Data.Array
import Data.Vect.Double
import qualified Data.Map as Map
import Data.Bullet.Actual
import MonadicShooter.Shell
import MonadicShooter.Field
import MonadicShooter.Concrete.Fluid
import MonadicShooter.Graphic
import Data.Functor.Identity

forever' a = let a' = a >> a' in a'
 
globalBarrage :: Monad m => Map.Map String Picture -> DanmakuT (Shell (ReaderT (Vec2 -> Vec2) m)) (Vec2 -> Vec2) (ReaderT Vec2 m) ()
globalBarrage m = mapFire (Just . createShell m ("wedge-blue", 5)) $ fluid
{-
ignoreReader :: DanmakuT (ActualBullet ()) Identity () -> DanmakuT (ActualBullet ()) (Reader Vec2) ()
ignoreReader danmaku = do
    (cont, xs) <- lift $ lift $ evolveDanmakuT danmaku
    mapM_ fire xs
    tick
    either ignoreReader (const $ return ()) cont
-}
fluid :: Monad m => DanmakuT (ActualBulletT (ReaderT (Vec2 -> Vec2) m) ()) (Vec2 -> Vec2) (ReaderT r m) ()
fluid = do
    forM_ ((,) <$> [12,36..480] <*> [12,36..480]) $ \(x,y) -> fire $ boundBy inTheField $ bullet (Vec2 x y)
    dispatch computeFluid 0 1
    where
        n = 100
        bullet :: Monad m => Vec2 -> ActualBulletT (ReaderT (Vec2 -> Vec2) m) ()
        bullet pos = do
            field <- ask
            let v = field (pos &* (1/480)) &* 3500
            yield (pos, angle2 v)
            bullet (pos &+ v)
        
        dispatch c n d = do
            Left (Yield x cont) <- runBulletT c `runReaderT` (4.2e-4 * d) 
            tick x
            if n == 720
                then dispatch cont 0 (-d)
                else dispatch cont (succ n) d
{-
youmu = forever $ youmu0 >> youmu1 >> wait 120

youmu0 :: DanmakuT Shell' (Reader Vec2) ()
youmu0 = mapShot (either (const Nothing) Just) $ flatten $ do
    fire $ gen (sine (-3)) ("circle-cyan", 7) 0
    fire $ gen (sine 3) ("circle-yellow", 7) 3
    tick
    wait 360
    where
        gen orbit color offset = embedToBullet orbit
            $ embedDanmakuState 0
            $ forever $ do
            n <- get
            (center, _) <- ask
            wait offset
            forM_ [0..n] $ fire . createShell color
                . boundBy (inRect (Vec2 0 (-180)) lowerRight)
                . \i -> uniformBullet ((i - n/2) / 20 * 2 * pi + pi / 2) 1.5 center
            wait 6
            put $ n + 1
        
        sine d = takeBullet 270
            $ embedBulletState (pi / 2)
            $ bulletWithVelocity (Vec2 240 240)
            $ forever $ do
            angle <- get
            yield $ Vec2 (sin angle * d) (-2.5)
            put $ angle + pi / 80
        
youmu1 :: DanmakuT Shell' (Reader Vec2) ()
youmu1 = mapShot (either (const Nothing) Just) $ flatten $ replicateM_ 2 $ do
    playerPos <- ask

    forM_ [Vec2 (-160) 0, Vec2 0 0, Vec2 160 0]
        $ fire . gen ("wedge-blue", 5) (uniformBullet (pi+pi/16) 18 $ Vec2 400 120)
          . (playerPos &+)
    wait 40
    
    forM_ [Vec2 (-240) 0, Vec2 (-80) 0, Vec2 80 0, Vec2 240 0]
        $ fire . gen ("wedge-red", 5) (uniformBullet (-pi/16) 18 $ Vec2 80 120)
          . (playerPos &+)
    wait 40

    where
        gen img orbit target = embedToBullet orbit $ forever $ do
            (center, _) <- ask
            forM_ [2,4..8] $ \speed -> fire $ createShell img $ boundBy inTheField
                $ uniformBullet (angle2 $ target &- center) speed center
            wait 2
            -}