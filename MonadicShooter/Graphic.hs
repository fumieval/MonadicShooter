module MonadicShooter.Graphic where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import System.FilePath.Windows
import MonadicShooter.DXFI

loadImage :: FilePath -> String -> IO (String, Handle)
loadImage path name = do
    h <- withCWString ("images" </> path) dxfi_LoadImage
    return (name, h)

loadImageByRects :: FilePath -> [(String, (Int, Int, Int, Int))] -> IO [(String, Handle)]
loadImageByRects path xs = do
    handle <- withCWString ("images" </> path) dxfi_LoadImage
    forM xs $ \(name, (w, h, x, y)) -> (,) name <$> dxfi_CropImage x y w h handle
