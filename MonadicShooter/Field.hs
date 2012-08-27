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

inTheField :: Vec2 -> Bool
inTheField (Vec2 x y) = x > leftBound && x < rightBound && y > upperBound && y < lowerBound