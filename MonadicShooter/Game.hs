module MonadicShooter.Game where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Vect
import qualified Data.Map as Map
import MonadicShooter.DXFI
import MonadicShooter.Graphic
import MonadicShooter.Bullet
import MonadicShooter.Input
import MonadicShooter.Player
import Data.Bullet
import Data.Danmaku
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
        ,theDanmaku :: DanmakuT RealBullet (Reader Vec2) ()
    }

barrage0 :: DanmakuT RealBullet (Reader Vec2) ()
barrage0 = forever $ do
    playerPos <- lift ask
    let a = angle2 (playerPos &- center)
    forM_ [0..39] $ \i ->
        fire $ RealBullet (sinCos (i / 40 * 2 * pi + a) &* 3) center 0
    wait 13
    where
        center = Vec2 240 120

barrage1 :: DanmakuT RealBullet (Reader Vec2) ()
barrage1 = embedDanmakuState 0 $ forever $ do
    playerPos <- lift $ lift $ ask
    angle <- lift get
    forM_ [0..49] $ \i ->
        fire $ RealBullet (sinCos (i / 50 * 2 * pi + angle) &* 2) center 1
    lift $ put (angle + pi / 100)
    
    wait 12
    where
        center = Vec2 240 60

getTheGame :: IO (Game TheInput TheState)
getTheGame = Game initState theInput theUpdate <$> fmap theOutput loadImages

initState :: IO TheState
initState = do
    gen <- getStdGen
    return $ Playing gen (Player (Vec2 240 360) PlayerNeutral 0) [] () (parallelDanmaku [barrage0, barrage1])

theUpdate :: TheInput -> TheState -> TheState
theUpdate input state = state
    { player = updatePlayer defaultPlayerSettings input (player state)
    , bullets = filter isActive (map updateBullet (bullets state)) ++ newBullets
    , theDanmaku = theDanmaku'
    }
    where
        (Just theDanmaku', newBullets) = evolveDanmakuT (theDanmaku state)
            `runReader` (playerPosition $ player state)
        
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