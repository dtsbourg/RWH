module GrahamScan where

data Direction = Left | Right | Straight deriving (Eq, Show)
type Vec2D = (Float, Float)

direction :: Vec2D -> Vec2D -> Direction
direction x y
	| alpha < pi / 2 = undefined
	| alpha > pi / 2 = undefined
	| otherwise = Straight

angle :: Vec2D -> Vec2D -> Float
angle (x1,y1) (x2,y2) = acos ((x1*x2)+(y1*y2))/(sqrt((x1*x1+y1*y1)*(x2*x2+y2*y2)))
