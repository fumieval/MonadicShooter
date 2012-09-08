{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Control.Monad
import Control.Applicative
import MonadicShooter.Input
import MonadicShooter.Game
import Foreign.C.String
import System.IO

foreign import ccall "DXFI_Initialize" dxfi_Initialize :: IO ()
foreign import ccall "DXFI_Release" dxfi_Release :: IO ()
foreign import ccall "DXFI_GetTickCount" dxfi_GetTickCount :: Bool -> IO Int
foreign import ccall "DXFI_SetLogging" dxfi_SetLogging :: Bool -> IO ()
foreign import ccall "DXFI_SetWindowMode" dxfi_SetWindowMode :: Int -> IO ()
foreign import ccall "DXFI_SetWindowCaption" dxfi_SetWindowCaption :: CWString -> IO ()
foreign import ccall "DXFI_SetDrawingDestination" dxfi_SetDrawingDestination :: Int -> IO ()
foreign import ccall "DXFI_Wait" dxfi_Wait :: Int -> IO ()
foreign import ccall "DXFI_ClearScreen" dxfi_ClearScreen :: IO ()
foreign import ccall "DXFI_FlipScreen" dxfi_FlipScreen :: IO ()
foreign import ccall "DXFI_AcceptMessage" processMessage  :: IO Int

setWindowCaption :: String -> IO ()
setWindowCaption = flip withCWString dxfi_SetWindowCaption

mainLoop game startTime t = do
    
    dxfi_ClearScreen
    cont <- runGame game
    dxfi_FlipScreen
    
    time <- dxfi_GetTickCount False
    dxfi_Wait $ (t * 1000) `div` 60 - time + startTime
    
    (startTime', t') <- if time - startTime >= 1000
        then setWindowCaption ("MonadicShooter [FPS:" ++ show (succ t) ++ "]") >> return (time, 0)
        else return (startTime, succ t)
    
    p <- processMessage
    esc <- dxfi_IsKeyPressed 0x01
    
    case cont of
        Just g -> when (p == 0 && esc == 0) $ mainLoop g startTime' t'
        Nothing -> return ()
    
main = do
    initialize
    
    game <- newGame
    startTime <- dxfi_GetTickCount False
    
    setWindowCaption "MonadicShooter [Done.]"
    mainLoop game startTime 0
    
    terminate

initialize = do
    dxfi_SetWindowMode 1 -- windowed
    dxfi_SetLogging False
    setWindowCaption "MonadicShooter [Initializing...]"
    dxfi_Initialize
    dxfi_SetDrawingDestination (-2) -- DX_SCREEN_BACK

terminate = dxfi_Release
