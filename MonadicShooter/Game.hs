module MonadicShooter.Game where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Vect
import qualified Data.Map as Map
import MonadicShooter.Graphic
import MonadicShooter.Bullet
import MonadicShooter.Player
import MonadicShooter.Input
import Data.Danmaku
import System.Random
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
data TheState = Playing
    {
        randomGen :: StdGen
        ,inputState :: TheInput
        ,player :: Player
		,bullets :: [RealBullet]
		,background :: ()
        ,theDanmaku :: DanmakuT RealBullet (Reader Vec2) ()
    }

globalInput :: Event -> TheState -> IO TheState
globalInput ev state = do
    inputState' <- return $ keyInput ev $ inputState state
    return $ state { inputState = inputState' }

globalUpdate :: Float -> TheState -> IO TheState
globalUpdate _ state = do
    let state' = state {
          player = updatePlayer defaultPlayerSettings (inputState state) (player state)
        , bullets = filter isActive (map updateBullet (bullets state)) ++ newbullets
        , theDanmaku = theDanmaku'
        }
    globalSound state'
    return state'
    where
        Just (newbullets, theDanmaku') = execDanmakuT (theDanmaku state) `runReader` (playerPosition $ player state)
        
initialState :: IO TheState
initialState = do
    gen <- getStdGen
    return $ Playing
        `id` gen
        `id` neutralInput
        `id` Player (Vec2 0 0) PlayerNeutral 0
        `id` []
        `id` ()
        `id` parallelDanmaku [barrage0, barrage1]

globalDraw :: Map.Map String Picture -> TheState -> IO Picture
globalDraw m state = return $ Pictures $
    [drawBackground m $ background state
    ,drawPlayer defaultPlayerSettings m $ player state
    ,Pictures $ map (drawBullet m) (bullets state)
    ]

globalSound :: TheState -> IO ()
globalSound _ = return ()

drawBackground :: Map.Map String Picture -> () -> Picture
drawBackground m _ = Pictures $
    [m Map.! "background"]

loadImages :: IO (Map.Map String Picture)
loadImages = Map.fromList <$> concat <$> sequence [loadImageByRects "images/Shot.png" [
              ("wedge-red", (19, 19, 1, 42))
              ,("wedge-green", (19, 19, 21, 42))
              ,("wedge-blue", (19, 19, 41, 42))
              ,("wedge-yellow", (19, 19, 61, 42))]
        ,return <$> loadImage "images/background-80px-grid.png" "background"
        ,loadImageByRects "images/sanae.png" [
            ("sanaeC0", (48, 48, 0, 0))
            ,("sanaeC1", (48, 48, 48, 0))
            ,("sanaeL0", (48, 48, 0, 48))
            ,("sanaeL1", (48, 48, 48, 48))
            ,("sanaeR0", (48, 48, 0, 96))
            ,("sanaeR1", (48, 48, 48, 96))
            ]
        ]
         
gameMain = do
    state <- initialState
    images <- loadImages
    playIO
        `id` InWindow "MonadicShooter" (640, 480) (100, 100)
        `id` makeColor 0 0 0 1
        `id` 60
        `id` state
        `id` globalDraw images
        `id` globalInput
        `id` globalUpdate

barrage0 :: DanmakuT RealBullet (Reader Vec2) ()
barrage0 = forever $ do
    playerPos <- lift ask
    let center = Vec2 0 (120)
    let a = angle2 (playerPos &- center)
    forM_ [0..59] $ \i ->
        fire $ RealBullet (sinCos (i / 60 * 2 * pi + a) &* 3) center "wedge-red"
    wait 30

barrage1 :: DanmakuT RealBullet (Reader Vec2) ()
barrage1 = forever $ do
    playerPos <- lift ask
    let center = Vec2 0 (180)
    forM_ [0..49] $ \i ->
        fire $ RealBullet (sinCos (i / 50 * 2 * pi) &* 2) center "wedge-blue"
    wait 27