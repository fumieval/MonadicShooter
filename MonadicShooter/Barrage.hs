module MonadicShooter.Barrage (globalBarrage) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import MonadicShooter.Shell
import Data.Vect
import Data.Danmaku
import Data.Bullet.Actual

globalBarrage = parallelDanmaku [barrage0, barrage1, barrage2]

barrage0 :: DanmakuT RealBullet (Reader Vec2) ()
barrage0 = forever $ do
    playerPos <- ask
    let a = angle2 (playerPos &- center)
    forM_ [0..29] $ \i ->
        fire $ uniformBullet ((i + 0.5) / 30 * 2 * pi + a) 3 center `realBullet` 0
    wait 30
    where
        center = Vec2 240 80

barrage1 :: DanmakuT RealBullet (Reader Vec2) ()
barrage1 = embedDanmakuState 0 $ forever $ do
    angle <- get
    forM_ [0..9] $ \i ->
        fire $ uniformBullet (i / 40 * 2 * pi + angle) 3 center `realBullet` 1
    put (angle + pi / 15)
    wait 12
    where
        center = Vec2 200 120

barrage2 :: DanmakuT RealBullet (Reader Vec2) ()
barrage2 = embedDanmakuState (pi/2) $ forever $ do
    angle <- get
    forM_ [0..9] $ \i ->
        fire $ uniformBullet (i / 40 * 2 * pi + angle) 3 center `realBullet` 2
    put (angle - pi / 15)
    wait 12
    where
        center = Vec2 280 120