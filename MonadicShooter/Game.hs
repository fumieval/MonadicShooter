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
        (Left barrage', newBullets) = evolveDanmakuT (barrage state)
            `runReader` playerPosition (player state)
        
outputBackground :: Map.Map String Handle -> () -> IO ()
outputBackground m _ = do
    -- background image
    dxfi_DrawImage 0 0 (m Map.! "background-80px-grid") False
    -- background music
    return ()
    
theOutput :: Map.Map String Handle -> TheState -> IO ()
theOutput m state = do
    outputBackground m                       $ background state
    outputPlayer defaultPlayerSettings m     $ player state
    mapM_ (outputBullet m) $ bullets state

loadImages :: IO (Map.Map String Handle)
loadImages = Map.fromList <$> concat <$> sequence [loadImageByRects "Shot.png" [
              ("wedge-red",     (19, 19, 1, 42))
              ,("wedge-green",  (19, 19, 21, 42))
              ,("wedge-blue",   (19, 19, 41, 42))
              ,("wedge-yellow", (19, 19, 61, 42))
              ,("wedge-pink",   (19, 19, 81, 42))
              ,("wedge-cyan",   (19, 19, 101, 42))
              ,("wedge-white",  (19, 19, 121, 42))
              ,("wedge-orange", (19, 19, 141, 42))
              ,("circle-red",     (19, 19, 1, 1))
              ,("circle-green",  (19, 19, 21, 1))
              ,("circle-blue",   (19, 19, 41, 1))
              ,("circle-yellow", (19, 19, 61, 1))
              ,("circle-pink",   (19, 19, 81, 1))
              ,("circle-cyan",   (19, 19, 101, 1))
              ,("circle-white",  (19, 19, 121, 1))
              ,("circle-orange", (19, 19, 141, 1))
              ]
        ,return <$> loadImage "background-80px-grid.png" "background-80px-grid"
        ,loadImageByRects "sanae.png" [
            ("sanaeC0", (48, 48, 0, 0))
            ,("sanaeC1", (48, 48, 48, 0))
            ,("sanaeL0", (48, 48, 0, 48))
            ,("sanaeL1", (48, 48, 48, 48))
            ,("sanaeR0", (48, 48, 0, 96))
            ,("sanaeR1", (48, 48, 48, 96))
            ]
        ]
         