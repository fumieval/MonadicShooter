module MonadicShooter.Game where

import Control.Applicative
import Control.Monad
import Control.Monad.Coroutine
import Data.Vect
import qualified Data.Map as Map
import MonadicShooter.DXFI
import MonadicShooter.Graphic
import MonadicShooter.Bullet
import MonadicShooter.Input
import MonadicShooter.Player
import Data.Functor.Identity
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
        ,theDanmaku :: DanmakuT Vec2 RealBullet Identity
    }

barrage0 :: DanmakuT Vec2 RealBullet Identity
barrage0 = forever $ do
    playerPos <- observe
    let center = Vec2 240 120
    let a = angle2 (playerPos &- center)
    forM_ [0..59] $ \i ->
        fire $ RealBullet (sinCos (i / 60 * 2 * pi + a) &* 4) center 0
    wait 30

barrage1 :: DanmakuT Vec2 RealBullet Identity
barrage1 = forever $ do
    playerPos <- observe
    let center = Vec2 240 120
    forM_ [0..49] $ \i ->
        fire $ RealBullet (sinCos (i / 50 * 2 * pi) &* 3) center 1
    wait 12

getTheGame :: IO (Game TheInput TheState)
getTheGame = Game initState theInput theUpdate <$> fmap theOutput loadImages

initState :: IO TheState
initState = do
    gen <- getStdGen
    return $ Playing gen (Player (Vec2 240 360) PlayerNeutral 0) [] () (parallelDanmaku [barrage0, barrage1])

theUpdate :: TheInput -> TheState -> TheState
theUpdate input state = state
    { player = updatePlayer defaultPlayerSettings input (player state)
    , bullets = filter isActive (map updateBullet (bullets state)) ++ newbullets
    , theDanmaku = theDanmaku'
    }
    where
        Just (newbullets, theDanmaku') = runIdentity $ runDanmaku (playerPosition $ player state) (theDanmaku state)

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