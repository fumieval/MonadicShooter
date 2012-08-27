module MonadicShooter.Game where

import Control.Applicative
import Data.Vect
import qualified Data.Map as Map
import MonadicShooter.DXFI
import MonadicShooter.Graphic
import MonadicShooter.Input
import MonadicShooter.Player
import MonadicShooter.Shell
import MonadicShooter.Barrage
import Data.Danmaku
import Control.Monad
import Control.Monad.Reader
import System.Random

data Game input state = Game {
    initialState :: IO state
    ,acquireInput :: IO input
    ,update :: input -> state -> state
    ,output :: state -> IO ()
    }

runGame :: Game i s -> s -> IO s
runGame game state = do
    input <- acquireInput game
    let state' = update game input state
    output game state'
    return state'

data TheState = Playing
    {
        randomGen :: StdGen
        ,player :: Player
		,bullets :: [RealBullet]
		,background :: ()
        ,barrage :: DanmakuT RealBullet (Reader Vec2) ()
    }


getTheGame :: IO (Game TheInput TheState)
getTheGame = Game initState theInput theUpdate <$> fmap theOutput loadImages

initState :: IO TheState
initState = do
    gen <- getStdGen
    return $ Playing gen (Player (Vec2 240 360) PlayerNeutral 0) [] () globalBarrage

theUpdate :: TheInput -> TheState -> TheState
theUpdate input state = state
    { player = updatePlayer defaultPlayerSettings input (player state)
    , bullets = filter isActive (map updateBullet (bullets state ++ newBullets))
    , barrage = barrage'
    }
    where
        (Just barrage', newBullets) = evolveDanmakuT (barrage state)
            `runReader` playerPosition (player state)
        
outputBackground :: ImageSet -> () -> IO ()
outputBackground m _ = do
    -- background image
    dxfi_DrawImage 0 0 (m Map.! Entire "background-80px-grid.png") False
    -- background music
    return ()
    
theOutput :: ImageSet -> TheState -> IO ()
theOutput m state = do
    outputBackground m                       $ background state
    outputPlayer defaultPlayerSettings m     $ player state
    mapM_ (outputBullet m) $ bullets state

loadImages :: IO ImageSet
loadImages = loader Map.empty
    where
        loader = loadImage "background-80px-grid.png"
            >=> loadImageByRect "Shot.png" 19 19 1 42
            >=> loadImageByRect "Shot.png" 19 19 21 42
            >=> loadImageByRect "Shot.png" 19 19 41 42
            >=> loadImageByRect "Shot.png" 19 19 61 42
            >=> loadImageMatrix "sanae.png" 48 48 2 3