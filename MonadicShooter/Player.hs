module MonadicShooter.Player where

import qualified Data.Map as Map
import Data.Vect.Double
import MonadicShooter.Input
import MonadicShooter.Graphic
import Control.Monad.Bullet
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.State.Strict

data PlayerSettings = PlayerSettings
    {
        playerSpeed :: Double
        ,playerSlowRate :: Double
        ,playerAnimationPeriod :: Int
        ,playerImage :: FilePath
        ,playerImageNeutral :: [String]
        ,playerImageLeft :: [String]
        ,playerImageRight :: [String]
        ,playerImageWidth :: Int
        ,playerImageHeight :: Int
    }

defaultPlayerSettings = PlayerSettings 4 0.5 20 "sanae.png" ["sanaeC0", "sanaeC1"] ["sanaeL0", "sanaeL1"] ["sanaeR0", "sanaeR1"] 48 48

createPlayer :: PlayerSettings
    -> Map.Map String Picture
    -> Vec2
    -> BulletT Vec2 (ReaderT TheInput (WriterT Picture IO)) ()
createPlayer settings images position = createPlayer' position 0 where
    createPlayer' pos n = do
        input <- ask
        let pos' = pos &+ v &* playerSpeed settings &* rate
            rate = if keyC input then playerSlowRate settings else 1
            (x, img) = case (keyLeft input, keyRight input) of
                (True, False) -> (-1, playerImageLeft settings)
                (False, True) -> (1, playerImageRight settings)
                _ -> (0, playerImageNeutral settings)
            y = case (keyUp input, keyDown input) of
                (True, False) -> -1
                (False, True) -> 1
                _ -> 0
            v | x == 0 && y == 0 = Vec2 0 0
              | otherwise = normalize (Vec2 x y)
            
            pic = images Map.! (img !! ((n * length img) `div` playerAnimationPeriod settings))
            o = Vec2 (fromIntegral (playerImageWidth settings) / 2) (fromIntegral (playerImageHeight settings) / 2)

        lift $ tell $ Translate (pos &- o) pic

        yield pos

        createPlayer' pos' (succ n `mod` playerAnimationPeriod settings)
