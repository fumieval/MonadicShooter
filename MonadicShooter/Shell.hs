module MonadicShooter.Shell where
import MonadicShooter.Graphic
import Control.Monad.Writer.Strict
import Control.Monad.Bullet
import Data.Bullet.Actual
import Data.Functor.Identity
import Data.Vect.Double
import qualified Data.Map as Map

type Shell = BulletT (Vec2, Double) (WriterT Picture IO) ()
type Shell' = Map.Map String Picture -> Shell

createShell :: (String, Double) -> ActualBullet () -> Shell'
createShell (img, r) bullet m = shell' bullet where
    pic = m Map.! img
    shell' b = case runIdentity $ runBulletT b of
        Left (Yield (pos, a) b') -> do
            lift $ tell $ Translate pos $ Rotate a pic
            yield (pos, r)
            shell' b'
        _ -> return ()