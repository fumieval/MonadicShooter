{-# LANGUAGE ForeignFunctionInterface #-}
module MonadicShooter.Graphic where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Vect.Double
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import System.FilePath.Windows

type Handle = CInt
foreign import ccall "DXFI_LoadImage" dxfi_LoadImage :: CWString -> IO Handle
foreign import ccall "DXFI_LoadImageMatrix" dxfi_LoadImageMatrix :: CWString -> Int -> Int -> Int -> Int -> Int -> Ptr Handle -> IO ()
foreign import ccall "DXFI_CropImage" dxfi_CropImage :: Int -> Int -> Int -> Int -> Handle -> IO Handle
foreign import ccall unsafe "DXFI_DrawImage" dxfi_DrawImage :: Int -> Int -> Handle -> Bool -> IO ()
foreign import ccall unsafe "DXFI_DrawImageScaledWithAngle" dxfi_DrawImageBy :: Int -> Int -> Double -> Double -> Handle -> Bool -> Bool -> IO ()

loadImage :: (Handle -> Picture) -> FilePath -> String -> IO (String, Picture)
loadImage f path name = do
    h <- withCWString ("images" </> path) dxfi_LoadImage
    return $! (name, f h)

loadImageByRects :: FilePath -> [(Handle -> Picture, String, (Int, Int, Int, Int))] -> IO [(String, Picture)]
loadImageByRects path xs = do
    handle <- withCWString ("images" </> path) dxfi_LoadImage
    forM xs $! \(f, name, (w, h, x, y)) -> (,) name <$> f <$> dxfi_CropImage x y w h handle

data Picture = ImageD {-# UNPACK #-} !(Int, Int) {-# UNPACK #-} !Handle
    | Image {-# UNPACK #-} !Handle
    | Pictures (Seq.Seq Picture)
    | Rotate {-# UNPACK #-} !Double Picture
    | Scale {-# UNPACK #-} !Double Picture
    | Translate Vec2 Picture 
    deriving Show

instance Monoid Picture where
    mempty = Pictures Seq.empty
    mappend (Pictures xs) (Pictures ys) = Pictures $ xs Seq.>< ys
    mappend x (Pictures ys) = Pictures $ x Seq.<| ys
    mappend (Pictures xs) y = Pictures $ xs Seq.|> y
    
drawPicture :: Picture -> IO ()
drawPicture = draw (zero, 1, 0) where
    draw (Vec2 x y, _, _) (ImageD (i, j) h) = dxfi_DrawImage (floor x - i) (floor y - i) h True
    draw (Vec2 x y, s, a) (Image h) = dxfi_DrawImageBy (floor x) (floor y) s a h True False
    draw (p, s, a) (Translate q x) = draw (p &+ q, s, a) x
    -- draw (Vec2 0 0, s, a) (Rotate t x) = draw (Vec2 0 0, s, a + t) x
    draw (p, s, a) (Rotate t x) = draw (p, s, a + t) x
    draw t (Pictures xs) = F.mapM_ (draw t) xs
    draw (p, s, a) (Scale k x) = draw (p &* k, s * k, a) x
