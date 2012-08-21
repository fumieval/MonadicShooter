{-# LANGUAGE ForeignFunctionInterface #-}
module MonadicShooter.DXFI where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

type Handle = CInt

foreign import ccall "DXFI_Initialize" dxfi_Initialize :: IO ()
foreign import ccall "DXFI_Release" dxfi_Release :: IO ()
foreign import ccall "DXFI_SetWindowMode" dxfi_SetWindowMode :: Int -> IO ()
foreign import ccall "DXFI_SetWindowCaption" dxfi_SetWindowCaption :: CWString -> IO ()
foreign import ccall "DXFI_SetDrawingDestination" dxfi_SetDrawingDestination :: Int -> IO ()
foreign import ccall "DXFI_Wait" dxfi_Wait :: Int -> IO ()
foreign import ccall "DXFI_ClearScreen" dxfi_ClearScreen :: IO ()
foreign import ccall "DXFI_FlipScreen" dxfi_FlipScreen :: IO ()
foreign import ccall "DXFI_AcceptMessage" processMessage  :: IO Int
foreign import ccall "DXFI_IsKeyPressed" dxfi_IsKeyPressed :: Int -> IO Int
foreign import ccall "DXFI_GetTickCount" dxfi_GetTickCount :: Bool -> IO Int
foreign import ccall "DXFI_LoadImage" dxfi_LoadImage :: CWString -> IO Handle
foreign import ccall "DXFI_LoadImageMatrix" dxfi_LoadImageMatrix :: CWString -> Int -> Int -> Int -> Int -> Int -> Ptr Handle -> IO ()
foreign import ccall "DXFI_CropImage" dxfi_CropImage :: Int -> Int -> Int -> Int -> Handle -> IO Handle
foreign import ccall "DXFI_DrawImage" dxfi_DrawImage :: Int -> Int -> Handle -> Bool -> IO ()
foreign import ccall "DXFI_DrawImageScaledWithAngle" dxfi_DrawImageBy :: Int -> Int -> Double -> Double -> Handle -> Bool -> Bool -> IO ()
foreign import ccall "DXFI_SetLogging" dxfi_SetLogging :: Bool -> IO ()

setWindowCaption :: String -> IO ()
setWindowCaption = flip withCWString dxfi_SetWindowCaption
