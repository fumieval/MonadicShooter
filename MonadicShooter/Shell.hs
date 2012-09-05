module MonadicShooter.Shell where
import MonadicShooter.DXFI
import MonadicShooter.Field
import GHC.Float
import Data.Vect
import Data.Bullet.Actual
import Data.Functor.Identity
import Debug.Trace
import qualified Data.Map as Map

data RealBullet = RealBullet String (ActualBullet ()) Bool Bool PosAndArg | Gone

realBullet :: String -> ActualBullet () -> RealBullet
realBullet a b = RealBullet a b True True undefined

realBulletS :: String -> ActualBullet () -> RealBullet
realBulletS a b = RealBullet a b True False undefined

phantomBullet :: String -> ActualBullet () -> RealBullet
phantomBullet a b = RealBullet a b False True undefined

updateBullet :: RealBullet -> RealBullet
updateBullet (RealBullet c b f g _) = case runIdentity $ runBulletT b of
    Left (Yield p@(pos, _) b') -> RealBullet c b' f g p
    _ -> Gone
updateBullet Gone = Gone

isActive Gone = False
isActive _ = True

outputBullet :: Map.Map String Handle -> RealBullet -> IO ()
outputBullet m (RealBullet i _ _ True (Vec2 x y, a)) = 
    dxfi_DrawImageBy (floor $ x + 0.5) (floor $ y + 0.5) 1.0 (float2Double a + pi / 2) (m Map.! i) True False
outputBullet m (RealBullet i _ _ False (Vec2 x y, a)) = 
    dxfi_DrawImage (floor $ x + 0.5) (floor $ y + 0.5) (m Map.! i) True