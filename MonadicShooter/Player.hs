module MonadicShooter.Player where

import qualified Data.Map as Map
import Data.Vect
import MonadicShooter.Input
import MonadicShooter.Graphic
import Graphics.Gloss.Data.Picture

data PlayerSettings = PlayerSettings
    {
        playerSpeed :: Float
        ,playerSlowRate :: Float
        ,playerAnimationPeriod :: Int
        ,playerImageNeutral :: [String]
        ,playerImageLeft :: [String]
        ,playerImageRight :: [String]
        ,playerImageWidth :: Int
        ,playerImageHeight :: Int
    }

data Player = Player
    {
        playerPosition :: Vec2
        ,playerMotion :: PlayerMotion
        ,playerAnimationCounter :: Int
    }

data PlayerMotion = PlayerNeutral | PlayerLeft | PlayerRight

defaultPlayerSettings = PlayerSettings 4 0.5 20 ["sanaeC0", "sanaeC1"] ["sanaeL0", "sanaeL1"] ["sanaeR0", "sanaeR1"] 48 48

updatePlayer :: PlayerSettings -> TheInput -> Player -> Player
updatePlayer settings input (Player pos _ n) = Player pos' m'
    $ mod (n + 1) (playerAnimationPeriod settings)
    where
        pos' = pos &+ v &* playerSpeed settings &* rate
        rate = if keyC input then playerSlowRate settings else 1
        
        (x, m') = case (keyLeft input, keyRight input) of
            (True, False) -> (-1, PlayerLeft)
            (False, True) -> (1, PlayerRight)
            _ -> (0, PlayerNeutral)
        y = case (keyUp input, keyDown input) of
            (True, False) -> 1
            (False, True) -> -1
            _ -> 0
        v | x == 0 && y == 0 = Vec2 0 0
          | otherwise = normalize (Vec2 x y)

drawPlayer :: PlayerSettings -> Map.Map String Picture -> Player -> Picture
drawPlayer settings images (Player (Vec2 x y) m n) = Translate x y
    $ images Map.! (img !! ((n * length img) `div` playerAnimationPeriod settings))
    where
       img = case m of
            PlayerNeutral -> playerImageNeutral settings
            PlayerLeft -> playerImageLeft settings
            PlayerRight -> playerImageRight settings