module MonadicShooter.Graphic where
import Control.Arrow
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Graphics.Gloss.Data.Picture
import System.FilePath.Windows
import Codec.Picture.Repa
import Data.Array.Repa as R hiding (map)
import qualified Data.Array.Repa.Repr.ForeignPtr as RF

loadImage :: FilePath -> String -> IO (String, Picture)
loadImage path name = do
    img <- imgData <$> either error id <$> readImageRGBA path
    let Z :. h :. w :. _ = R.extent img
    return (name, bitmapOfForeignPtr w h (RF.toForeignPtr $ flipVertically img) True)

loadImageByRects :: FilePath -> [(String, (Int, Int, Int, Int))] -> IO [(String, Picture)]
loadImageByRects path xs = do
    img <- imgData <$> either error id <$> readImageRGBA path
    return $ map (second $ ($img) . crop) xs
    where
        crop (w, h, x, y) r = flip (bitmapOfForeignPtr w h) True
            $ RF.toForeignPtr $ flipVertically $ R.computeS
            $ R.extract (Z :. y :. x :. 0) (Z :. h :. w :. d) r
            where
                Z :. _ :. _ :. d = R.extent r
