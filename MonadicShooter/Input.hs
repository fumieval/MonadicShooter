{-# LANGUAGE ForeignFunctionInterface #-}
module MonadicShooter.Input where
import Data.Bits
import Graphics.Gloss.Interface.IO.Game
import Foreign.C.Types
import qualified Graphics.UI.GLUT.Callbacks.Window as GLUT
data TheInput = TheInput 
    {    
        keyLeft :: Bool
        ,keyRight :: Bool
        ,keyUp :: Bool
        ,keyDown :: Bool
        ,keyA :: Bool
        ,keyB :: Bool
        ,keyC :: Bool
    }

neutralInput :: TheInput
neutralInput = TheInput False False False False False False False
{-
applyModifiers :: TheInput -> IO TheInput
applyModifiers state = do
    m <- glutGetModifiers
    return $ state { keyC = testBit m 0}
-}
keyInput :: Event -> TheInput -> TheInput
keyInput (EventKey (Char 'Z') s _ _) state = state { keyA = s == Down }
keyInput (EventKey (Char 'X') s _ _) state = state { keyB = s == Down }
keyInput (EventKey (Char 'A') s _ _) state = state { keyC = s == Down }
keyInput (EventKey (SpecialKey KeyUp) s _ _) state = state { keyUp = s == Down }
keyInput (EventKey (SpecialKey KeyDown) s _ _) state = state { keyDown = s == Down }
keyInput (EventKey (SpecialKey KeyLeft) s _ _) state = state { keyLeft = s == Down }
keyInput (EventKey (SpecialKey KeyRight) s _ _) state = state { keyRight = s == Down }
keyInput _ state = state