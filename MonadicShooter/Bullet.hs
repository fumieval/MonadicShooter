module MonadicShooter.Bullet where
import MonadicShooter.Graphic
import MonadicShooter.DXFI
import MonadicShooter.Field
import Control.Monad.Trans.State
import GHC.Float
import Data.Vect
import Data.Danmaku
import qualified Data.Map as Map

data RealBullet = RealBullet Vec2 Vec2 Int | Gone

updateBullet :: RealBullet -> RealBullet
updateBullet (RealBullet v p@(Vec2 x y) c)
    | x < leftBound || x > rightBound || y < upperBound || y > lowerBound = Gone
    | otherwise = RealBullet v (p &+ v) c
updateBullet Gone = Gone

isActive Gone = False
isActive _ = True

outputBullet :: ImageSet -> RealBullet -> IO ()
outputBullet m (RealBullet v (Vec2 x y) c) = 
    dxfi_DrawImageBy (floor x) (floor y) 1.0 (float2Double $ angle2 v + pi / 2) (m Map.! Crop "Shot.png" 19 19 (1 + 20 * c) 42) True False
