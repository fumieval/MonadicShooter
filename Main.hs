module Main where
import Control.Monad
import Control.Applicative

import MonadicShooter.Game
import MonadicShooter.DXFI
import Foreign.C.String
import System.IO

mainLoop game state startTime t = do
    
    dxfi_ClearScreen
    state' <- runGame game state
    dxfi_FlipScreen
    
    time <- dxfi_GetTickCount False
    dxfi_Wait $ (t * 1000) `div` 60 - time + startTime
    
    (startTime', t') <- if time - startTime >= 1000
        then setWindowCaption ("MonadicShooter [FPS:" ++ show (succ t) ++ "]") >> return (time, 0)
        else return (startTime, succ t)
    
    p <- processMessage
    esc <- dxfi_IsKeyPressed 0x01
    
    when (p == 0 && esc == 0) $ mainLoop game state' startTime' t'

main :: IO ()
main = do
    initialize
    
    game <- getTheGame
    state <- initialState game
    startTime <- dxfi_GetTickCount False
    
    setWindowCaption "MonadicShooter [Done.]"
    mainLoop game state startTime 0
    
    terminate

initialize = do
    dxfi_SetWindowMode 1 -- windowed
    dxfi_SetLogging False
    setWindowCaption "MonadicShooter [Initializing...]"
    dxfi_Initialize
    dxfi_SetDrawingDestination (-2) -- DX_SCREEN_BACK

terminate = dxfi_Release
