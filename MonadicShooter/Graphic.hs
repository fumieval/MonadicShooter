module MonadicShooter.Graphic where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import System.FilePath.Windows
import MonadicShooter.DXFI

data ImageRef = Entire FilePath | Crop FilePath Int Int Int Int | Matrix FilePath (Int, Int) deriving (Eq, Ord)

type ImageSet = Map.Map ImageRef Handle

loadImage :: FilePath -> ImageSet -> IO ImageSet
loadImage path m
    | Entire path `Map.member` m = return m
    | otherwise = Map.insert (Entire path)
        <$> withCWString ("images" </> path) dxfi_LoadImage
        <*> pure m

loadImageByRect :: FilePath -> Int -> Int -> Int -> Int -> ImageSet -> IO ImageSet
loadImageByRect path w h x y m_ = do
    m <- loadImage path m_
    handle <- dxfi_CropImage x y w h $ m Map.! Entire path
    return $ Map.insert (Crop path w h x y) handle m

loadImageMatrix :: FilePath -> Int -> Int -> Int -> Int -> ImageSet -> IO ImageSet
loadImageMatrix path w h c r m = do
    arr <- mallocArray (c * r)
    withCWString ("images" </> path) $ \p -> dxfi_LoadImageMatrix p (c * r) c r w h arr
    foldr (uncurry Map.insert) m
        <$> zip [Matrix path (i, j) | j <- [0..], i <- [0..c-1]]
        <$> peekArray 6 arr
