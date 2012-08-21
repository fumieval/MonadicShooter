module Data.Danmaku where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Coroutine
import Data.Maybe

type Danmaku i b = Coroutine (Firing b) (Reader i) ()

data Firing b c = Fire b c | Tick c

instance Functor (Firing x) where
    fmap f (Fire x y) = Fire x (f y)
    fmap f (Tick y) = Tick (f y)

instance Functor s => MonadTrans (Coroutine s) where -- 再定義しないとなぜか動かない
   lift = Coroutine . liftM Right

runDanmaku :: i -> Danmaku i b -> Maybe ([b], Danmaku i b)
runDanmaku input danmaku = case runReader (resume danmaku) input of
    Left (Fire x cont) -> first (x:) `liftM` runDanmaku input cont
    Left (Tick cont) -> Just ([], cont)
    _ -> Nothing

fire :: b -> Danmaku i b
fire x = suspend $ Fire x (return ())

tick :: Danmaku i b
tick = suspend $ Tick (return ())

wait = flip replicateM tick

observe :: Coroutine (Firing b) (Reader i) i
observe = lift ask

parallelDanmaku :: [Danmaku i b] -> Danmaku i b
parallelDanmaku xs = do
    input <- observe
    let (bss, ds) = unzip $ catMaybes $ map (runDanmaku input) xs
    mapM_ fire $ concat bss
    unless (null ds) $ tick >> parallelDanmaku ds
