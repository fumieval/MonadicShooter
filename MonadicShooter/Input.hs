{-# LANGUAGE ForeignFunctionInterface #-}
module MonadicShooter.Input where
import Control.Applicative

foreign import ccall "DXFI_IsKeyPressed" dxfi_IsKeyPressed :: Int -> IO Int

data TheInput = TheInput 
    {    
        keyLeft :: Bool
        ,keyRight :: Bool
        ,keyUp :: Bool
        ,keyDown :: Bool
        ,keyA :: Bool
        ,keyB :: Bool
        ,keyC :: Bool
    } deriving Show

getTheInput :: IO TheInput -- TODO: load configurations from file
getTheInput = TheInput <$> chk left <*> chk right <*> chk up <*> chk down <*> chk a <*> chk b <*> chk c
    where
        defaultConfig = (0xCB, 0xCD, 0xC8, 0xD0, 0x2C, 0x2D, 0x2A)
        (left, right, up, down, a, b, c) = defaultConfig
        chk x = do
            v <- dxfi_IsKeyPressed x
            return (v /= 0)