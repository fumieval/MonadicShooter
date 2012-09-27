module MonadicShooter.Shell where
import MonadicShooter.Graphic
import Control.Monad.Writer.Strict
import Control.Monad.Bullet
import Data.Bullet.Actual
import Data.Functor.Identity
import Data.Vect.Double
import qualified Data.Map as Map

type Shell m = BulletT (Vec2, Double) (WriterT Picture m) ()
-- TODO: introduce a typeclass
createShell :: Monad m => Map.Map String Picture -> (String, Double) -> ActualBulletT m () -> Shell m
createShell m (img, r) bullet = shell bullet where
    pic = m Map.! img
    shell b = do
        r <- lift $ lift $ runBulletT b
        case r of
            Left (Yield (pos, a) b') -> do
                lift $ tell $ Translate pos $ Rotate a pic
                yield (pos, a)
                shell b'
            _ -> return ()
