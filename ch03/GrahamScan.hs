-- implementation of Graham's Scan algorithm
import Data.List

data Direction = DirectionLeft | DirectionStraight | DirectionRight

data Point = Point Double Double

direction :: Point -> Point -> Point -> Direction
direction (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | d < 0  = DirectionRight
    | d == 0 = DirectionStraight
    | 0 < d  = DirectionLeft
  where d = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)


sumThree a b c = a + b + c

-- mapSuccessiveTriples :: [a] -> [b]
mapSuccessiveTriples :: (t -> t -> t -> t1) -> [t] -> [t1]
mapSuccessiveTriples f (a:b:c:[]) = [f a b c]
mapSuccessiveTriples f (a:b:c:xs) = f a b c : mapSuccessiveTriples f (b:c:xs)

-- step 1: find the point with the lowest y-coordinate. if multiple points have
-- the same lowest y-coordinate, find the point with the lowest x and
-- y-coordinate

-- this function finds the lowest point. it should be called
--    lowestPoint Nothing pointsToSearch
-- the first argument represents the lowest point for internal use
-- the second argument is the list of points to search
lowestPoint :: Maybe Point -> [Point] -> Point
lowestPoint Nothing (p:points) = lowestPoint (Just p) points
lowestPoint (Just p) [] = p
lowestPoint (Just (Point low_x low_y)) (Point x y : points)
  | low_y < y               = lowestPoint (Just (Point low_x low_y)) points
  | y < low_y               = lowestPoint (Just (Point x y)) points
  | low_y == y && low_x < x = lowestPoint (Just (Point low_x low_y)) points
  | low_y == y && x < low_x = lowestPoint (Just (Point x y)) points

-- step 2: sort the points in increasing order of the angle they and P make
-- with the x-axis
-- I think this means sort them by the angle of the line from P to A??
angleBetween :: Point -> Point -> Double
angleBetween (Point x1 y1) (Point x2 y2) = atan ((y2 - y1) / (x2 - x1))

sortByAngle :: Point -> [Point] -> [Point]


-- step 3: consider each point in the sorted array in sequence
--    determine the direction from the two previous points in the sequence
--    if the direction is a right turn, remove it


-- stub main function to quiet warnings
main :: IO ()
main = putStrLn "This module should be loaded."
