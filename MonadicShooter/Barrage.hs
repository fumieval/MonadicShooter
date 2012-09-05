module MonadicShooter.Barrage (globalBarrage) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Array
import Data.Vect
import Data.Danmaku
import Data.Bullet
import Data.Bullet.Actual
import MonadicShooter.Shell
import MonadicShooter.Field

globalBarrage = youmu

youmu = forever $ youmu0 >> youmu1 >> wait 120

youmu0 :: DanmakuT RealBullet (Reader Vec2) ()
youmu0 = mapShot (either (const Nothing) Just) $ flatten $ do
    fire $ gen (sine (-3)) "circle-cyan" 0
    fire $ gen (sine 3) "circle-yellow" 2
    tick
    wait 360
    where
        gen orbit color offset = embedToBullet orbit
            $ embedDanmakuState 0
            $ forever $ do
            n <- get
            (center, _) <- ask
            wait offset
            forM_ [0..n] $ fire . realBulletS color
                . boundBy (inRect (Vec2 0 (-180)) lowerRight)
                . \i -> uniformBullet ((i - n/2) / 20 * 2 * pi + pi / 2) 1.5 center
            tick
            wait 4
            put $ n + 1
        
        sine d = takeBullet 270
            $ embedBulletState (pi / 2)
            $ bulletWithVelocity (Vec2 240 240)
            $ forever $ do
            angle <- get
            yield $ Vec2 (sin angle * d) (-2.5)
            put $ angle + pi / 80
        
youmu1 :: DanmakuT RealBullet (Reader Vec2) ()
youmu1 = mapShot (either (const Nothing) Just) $ flatten $ replicateM_ 2 $ do
    playerPos <- ask

    forM_ [Vec2 (-160) 0, Vec2 0 0, Vec2 160 0]
        $ fire . gen "wedge-blue" (uniformBullet (pi+pi/16) 18 $ Vec2 400 120)
          . (playerPos &+)
    wait 40
    
    forM_ [Vec2 (-240) 0, Vec2 (-80) 0, Vec2 80 0, Vec2 240 0]
        $ fire . gen "wedge-red" (uniformBullet (-pi/16) 18 $ Vec2 80 120)
          . (playerPos &+)
    wait 40

    where
        gen img orbit target = embedToBullet orbit $ forever $ do
            (center, _) <- ask
            forM_ [2,4,6,8] $ \speed -> fire $ realBullet img $ boundBy inTheField
                $ uniformBullet (angle2 $ target &- center) speed center
            wait 2