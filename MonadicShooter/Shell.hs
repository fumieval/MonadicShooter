module MonadicShooter.Shell where
import MonadicShooter.Graphic
import MonadicShooter.DXFI
import MonadicShooter.Field
import GHC.Float
import Data.Vect
import Data.Bullet.Actual
import Data.Functor.Identity
import Debug.Trace
import qualified Data.Map as Map

type Color = Int

data RealBullet = RealBullet (ActualBullet ()) Color PosAndArg | Gone

realBullet :: ActualBullet () -> Color -> RealBullet
realBullet a b = RealBullet a b undefined

updateBullet :: RealBullet -> RealBullet
updateBullet (RealBullet b c _) = case runIdentity $ runBulletT b of
    Just (Yield p@(pos, _) b')
        | inTheField pos -> RealBullet b' c p
        | otherwise -> Gone
    Nothing -> Gone
updateBullet Gone = Gone

isActive Gone = False
isActive _ = True

outputBullet :: ImageSet -> RealBullet -> IO ()
outputBullet m (RealBullet _ c (Vec2 x y, a)) = 
    dxfi_DrawImageBy (floor $ x + 0.5) (floor $ y + 0.5) 1.0 (float2Double a + pi / 2) (m Map.! Crop "Shot.png" 19 19 (1 + 20 * c) 42) True False
