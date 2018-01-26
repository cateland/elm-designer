module Math exposing (..)

import Mouse exposing (Position)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Components


postionToPoint2d : Position -> Point2d
postionToPoint2d position =
    Point2d.fromCoordinates ( toFloat position.x, toFloat position.y )


isVectorOver : Point2d -> Components.Shape -> Bool
isVectorOver point2d shape =
    case
        shape
    of
        Components.BoundingBox2d box ->
            BoundingBox2d.contains point2d box
