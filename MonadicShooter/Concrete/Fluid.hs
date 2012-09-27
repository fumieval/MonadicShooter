{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module MonadicShooter.Concrete.Fluid (Field, computeFluid, toVectorField) where

import Data.Array.Unsafe
import Data.Array.Unboxed
import Data.Array.MArray hiding (unsafeFreeze, unsafeThaw)
import Data.Array.ST hiding (unsafeFreeze, unsafeThaw)
import Control.Monad
import Control.Applicative
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Bullet
import Data.Vect.Double

dt  = 0.02
dr  = 0.01
cols = 16
rows =  16
nu = 0.3e-3
e = 1e-4
imm = cols-1
jmm = rows-1
imp = cols+1
jmp = rows+1
hdr = dr / 2

fieldRange = ((0,0), (cols+1, rows+1))

type VectorField = Vec2 -> Vec2

type MField s = STUArray s (Int,Int) Double
type Field = UArray (Int,Int) Double

getField :: Field
getField = listArray fieldRange $ repeat 0

computeFluid :: Monad m => BulletT VectorField (ReaderT Double m) ()
computeFluid = loop getField getField getField
    where
        loop p vx vy = do
            vW <- ask
            let (p', vx', vy') = compute p vx vy vW
            yield $! toVectorField (vx', vy')
            loop p' vx' vy'
            
        compute p_ vx_ vy_ vW = runST $ do
            let h = computeHalves vx_ vy_
            p <- thaw p_
            vx <- thaw vx_
            vy <- thaw vy_
            computePressure h p vx vy
            computeVelocity h p vx vy vW
            (,,) <$> freeze p <*> freeze vx <*> freeze vy
        
toVectorField :: (Field, Field) -> VectorField
toVectorField (vx, vy) (Vec2 x y) = Vec2 x' y'
    where
        i' = x * fromIntegral imm + 1
        j' = y * fromIntegral jmm + 1
        i = floor i'
        j = floor j'
        p = i' - fromIntegral i
        q = j' - fromIntegral j
        x' = vx ! (i, j) * (1 - p) * (1 - q) + vx ! (i + 1, j) * p * (1 - q) + vx ! (i, j + 1) * (1 - p) * q + vx ! (i + 1, j + 1) * p * q
        y' = vy ! (i, j) * (1 - p) * (1 - q) + vy ! (i + 1, j) * p * (1 - q) + vy ! (i, j + 1) * (1 - p) * q + vy ! (i + 1, j + 1) * p * q
        
computeHalves :: Field -> Field -> (Field, Field, Field, Field)      
computeHalves vx vy = (hvx, hvy, mvx, mvy) where
    hvx = (getField //) $ do
        ix@(i, j) <- range ((0,0),(cols+1, rows))
        return (ix, (vx ! (i, j+1) + vx ! ix) / 2)

    hvy = (getField //) $ do
        ix@(i, j) <- range ((0,0), (cols, rows+1))
        return (ix, (vy ! (i + 1, j) + vy ! ix) / 2)

    mvx = (getField //) $ do
        ix@(i, j) <- range ((0,0),(cols, rows))
        return ((i + 1, j), (vx ! (i + 1, j) + vx ! ix) / 2)

    mvy = (getField //) $ do
        ix@(i, j) <- range ((0,0),(cols, rows))
        return ((i, j + 1), (vy ! (i, j + 1) + vy ! ix) / 2)

computeVelocity :: (Field, Field, Field, Field) -> MField s -> MField s -> MField s -> Double -> ST s ()
computeVelocity (hvx, hvy, mvx, mvy) p_ vx_ vy_ vW = do
    
    p :: Field <- unsafeFreeze p_
    vx :: Field <- unsafeFreeze vx_
    vy :: Field <- unsafeFreeze vy_
    
    let vxs = do
            ix@(i, j) <- range ((1,1), (cols, cols-1))
            let vxx = mvx ! ix * mvx ! (i, j) - mvx ! (i-1,j) * mvx ! (i-1, j)
                vxy = hvx ! ix * hvy ! (i-1, j+1) - hvx ! (i, j-1) * hvy ! (i-1, j)
                v2x = (vx ! (i+1, j) - 2 * vx ! ix + vx ! (i-1,j)) / dr^2
                v2y = (vx ! (i,j+1) - 2 * vx ! ix + vx ! (i,j-1)) / dr^2
                pxy = (p ! ix - p ! (i-1, j)) / dr
                xx = vxx / dr + vxy / dr
            return (ix, vx ! ix + (xx - pxy + (v2x + v2y) * nu) * dt)
        
        vys = do
            ix@(i, j) <- range ((1,1), (cols-1, cols))
            let vyy = mvy ! ix * mvy ! (i, j) - mvy ! (i,j-1) * mvy ! (i, j-1)
                vxy = hvx ! ix * hvy ! (i+1, j-1) - hvx ! (i-1, j) * hvy ! (i, j-1)
                v2x = (vy ! (i+1, j) - 2 * vy ! ix + vy ! (i-1,j)) / dr^2
                v2y = (vy ! (i, j+1) - 2 * vy ! ix + vy ! (i,j-1)) / dr^2
                pxy = (p ! ix - p ! (i, j - 1)) / dr
                yy = vyy / dr + vxy / dr
            return (ix, vy ! ix + (yy - pxy + (v2x + v2y) * nu) * dt)

    forM_ vxs $ uncurry $ writeArray vx_
    forM_ vys $ uncurry $ writeArray vy_
    
    forM_ [1..rows - 1] $ \j -> do
        writeArray vx_ (  1, j) 0.0
        writeArray vx_ (cols, j) 0.0

        readArray vx_ (2, j) >>= writeArray vx_ (0, j) . negate
        readArray vx_ (imm, j) >>= writeArray vx_ (imp, j) . negate

        readArray vy_ (1, j) >>= writeArray vy_ (0, j)
        readArray vy_ (imm, j) >>= writeArray vy_ (cols, j)

    forM_ [1..cols - 1] $ \i -> do
        writeArray vy_ (i, 1) 0.0
        writeArray vy_ (i, rows) 0.0

        readArray vy_ (i, 2) >>= writeArray vx_ (i, 0) . negate
        readArray vy_ (i, jmm) >>= writeArray vx_ (i, jmp) . negate

        readArray vx_ (i, 1) >>= writeArray vx_ (i, 0)
        readArray vx_ (i, jmm) >>= writeArray vx_ (i, rows) . (+vW)

computePressure :: (Field, Field, Field, Field) -> MField s -> MField s -> MField s -> ST s ()
computePressure (hvx, hvy, mvx, mvy) p vx_ vy_ = do

    vx :: Field <- unsafeFreeze vx_
    vy :: Field <- unsafeFreeze vy_
    
    let dd = (getField //) $ do
            ix@(i, j) <- range ((1,1), (cols-1,rows-1))
            return (ix, (vx ! (i+1,j) - vx ! (i,j) + vy ! (i,j+1) - vy ! (i,j)) / dr)
        t :: Field
        t =  (getField //) $ do
            ix@(i, j) <- range ((1,1), (cols-1,rows-1))
            let v2x = mvx ! (i+1, j) ^ 2 - 2 * mvx ! (i, j) ^ 2 + mvx ! (i-1,j) ^ 2
                v2y = mvy ! (i, j+1) ^ 2 - 2 * mvy ! (i, j) ^ 2 + mvy ! (i,j-1) ^ 2
                vxy = hvx ! (i + 1, j) * hvy ! (i, j + 1) - hvx ! (i + 1, j - 1) * hvy ! (i, j)
                  - hvx ! (i,j) * hvy ! (i - 1, j + 1) + hvx ! (i, j - 1) * hvy ! (i - 1, j)
            return (ix, (v2x + 2 * vxy + v2y) - dd ! ix * dr * dr / dt
                - (dd ! (i + 1, j) + dd ! (i - 1, j) - 4 * dd ! ix + dd ! (i, j + 1) + dd ! (i, j - 1)) * nu)

    gaussSeidel t p

gaussSeidel t p = do
    err <- fmap sum . forM (range ((1,1), (cols-1, rows-1))) $ \ix@(i, j) -> do
        prev <- readArray p ix
        a <- readArray p (i, j - 1)
        b <- readArray p (i, j + 1)
        c <- readArray p (i - 1, j)
        d <- readArray p (i + 1, j)
        let v = (a + b + c + d + t ! ix) / 4
        writeArray p ix v
        return $ abs $ v - prev
  
    forM_ [1..rows-1] $ \j -> do
        writeArray p (0, j) =<< readArray p (1, j)
        writeArray p (cols, j) =<< readArray p (imm, j)
  
    forM_ [1..cols-1] $ \i -> do
        writeArray p (i, 0) =<< readArray p (i, 1)
        writeArray p (i, rows) =<< readArray p (i, jmm)

    when (err > e) $ gaussSeidel t p