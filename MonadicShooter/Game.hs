{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module MonadicShooter.Game where

import Control.Applicative
import Data.Vect.Double
import qualified Data.Map as Map
import MonadicShooter.Graphic
import MonadicShooter.Input
import MonadicShooter.Player
import MonadicShooter.Shell
import MonadicShooter.Barrage
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict
import Control.Monad.Danmaku
import Control.Monad.Bullet
import System.Random
import Data.Maybe

newGame :: IO (BulletT () (ReaderT TheInput (WriterT Picture IO)) ())
newGame = do
    images <- loadImages
    return $ createGame images (Vec2 240 360) (globalBarrage images) 3
  
runGame :: BulletT () (ReaderT TheInput (WriterT Picture IO)) ()
    -> IO (Maybe (BulletT () (ReaderT TheInput (WriterT Picture IO)) ()))
runGame game = do
    input <- getTheInput
    (r, w) <- runWriterT $ runBulletT game `runReaderT` input 
    drawPicture w
    case r of
        Left (Yield _ cont) -> return (Just cont)
        Right _ -> return Nothing


createGame :: Map.Map String Picture
     -> Vec2
     -> DanmakuT (Shell (ReaderT r IO)) r (ReaderT Vec2 IO) b
     -> Int
     -> BulletT () (ReaderT TheInput (WriterT Picture IO)) ()
createGame images initialPlayerPos initialDanmaku initialRest = game [] (executeDanmaku initialDanmaku) newPlayer initialRest where
    game shells danmaku player rest = do
        
        tell $ images Map.! "background-80px-grid"
        
        (player', rest', playerPos) <- do
            r <- lift $ runBulletT player
            case r of
                Left (Yield p cont) -> return (cont, rest, p)
                Right _ -> return (newPlayer, rest - 1, initialPlayerPos)
        
        (r, shells', pics) <- lift $ lift $ lift $ runRWST (runBulletT danmaku) playerPos shells
        let danmaku' = case r of
                Left (Yield _ cont) -> cont
                Right _ -> undefined
        
        tell pics
        
        yield ()
        
        game shells' danmaku' player' rest'
    
    newPlayer = createPlayer defaultPlayerSettings images initialPlayerPos

executeDanmaku :: Monad m => DanmakuT (Shell (ReaderT r m)) r (ReaderT Vec2 m) b
    -> BulletT () (RWST Vec2 Picture [Shell (ReaderT r m)] m) b
executeDanmaku danmaku = do
    playerPos <- ask
    shells :: [Shell (ReaderT r m)] <- get
    r <- lift $ lift $ evolveDanmakuT danmaku `runReaderT` playerPos
    case r of
        Left (Yield (newshells, d) cont) -> do
            shells' <- lift $ forM shells $ \shell -> do
                (r, w) <- lift $ runWriterT (runBulletT shell) `runReaderT` d
                tell w
                case r of
                    Left (Yield (pos, r) cont)
                        | distance pos playerPos < r -> return []
                        | otherwise -> return [cont]
                    Right _ -> return []
            put $ newshells ++ concat shells'
            yield ()
            executeDanmaku cont
        Right a -> return a

loadImages :: IO (Map.Map String Picture)
loadImages = Map.fromList <$> concat <$> sequence [loadImageByRects "Shot.png" [
               (b, "wedge-red",     (19, 19, 1, 42))
              ,(b, "wedge-green",  (19, 19, 21, 42))
              ,(b, "wedge-blue",   (19, 19, 41, 42))
              ,(b, "wedge-yellow", (19, 19, 61, 42))
              ,(b, "wedge-pink",   (19, 19, 81, 42))
              ,(b, "wedge-cyan",   (19, 19, 101, 42))
              ,(b, "wedge-white",  (19, 19, 121, 42))
              ,(b, "wedge-orange", (19, 19, 141, 42))
              ,(b_sym, "circle-red",     (19, 19, 1, 1))
              ,(b_sym, "circle-green",  (19, 19, 21, 1))
              ,(b_sym, "circle-blue",   (19, 19, 41, 1))
              ,(b_sym, "circle-yellow", (19, 19, 61, 1))
              ,(b_sym, "circle-pink",   (19, 19, 81, 1))
              ,(b_sym, "circle-cyan",   (19, 19, 101, 1))
              ,(b_sym, "circle-white",  (19, 19, 121, 1))
              ,(b_sym, "circle-orange", (19, 19, 141, 1))
              ]
        ,return <$> loadImage (ImageD (0,0)) "background-80px-grid.png" "background-80px-grid"
        ,loadImageByRects "sanae.png" [
            (ImageD (0, 0), "sanaeC0", (48, 48, 0, 0))
            ,(ImageD (0, 0), "sanaeC1", (48, 48, 48, 0))
            ,(ImageD (0, 0), "sanaeL0", (48, 48, 0, 48))
            ,(ImageD (0, 0), "sanaeL1", (48, 48, 48, 48))
            ,(ImageD (0, 0), "sanaeR0", (48, 48, 0, 96))
            ,(ImageD (0, 0), "sanaeR1", (48, 48, 48, 96))
            ]
        ]
    where
        b = Rotate (pi/2) . Image
        b_sym = ImageD (9, 9)
         