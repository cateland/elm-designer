module Math exposing (..)

import Mouse exposing (Position)
import Point2d as Point2d exposing (Point2d)


positionToPoint2d : Position -> Point2d
positionToPoint2d position =
    Point2d.fromCoordinates ( toFloat position.x, toFloat position.y )


