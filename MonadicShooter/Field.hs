module MonadicShooter.Field where
import Data.Vect

upperBound :: Float
upperBound = 0.0

leftBound :: Float
leftBound = 0.0

lowerBound :: Float
lowerBound = 480.0

rightBound :: Float
rightBound = 480.0

upperLeft :: Vec2
upperLeft = Vec2 leftBound upperBound

lowerRight :: Vec2
lowerRight = Vec2 rightBound lowerBound

inTheField :: Vec2 -> Bool
inTheField (Vec2 x y) = x > leftBound && x < rightBound && y > upperBound && y < lowerBound

inRect :: Vec2 -> Vec2 -> Vec2 -> Bool
inRect (Vec2 x0 y0) (Vec2 x1 y1) (Vec2 x y) = x > x0 && x < y1 && y > y0 && y < y1