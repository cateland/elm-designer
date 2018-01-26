module Math exposing (..)

import Mouse exposing (Position)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (contains, translateBy)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Components exposing (Shape(..))


type alias Drag =
    { startPos : Position
    , previousPos : Position
    , currentPos : Position
    }


postionToPoint2d : Position -> Point2d
postionToPoint2d position =
    Point2d.fromCoordinates ( toFloat position.x, toFloat position.y )


isVectorOver : Point2d -> Components.Shape -> Bool
isVectorOver point2d shape =
    case
        shape
    of
        BoundingBox2d box ->
            contains point2d box


translateBy : Vector2d -> Components.Shape -> Components.Shape
translateBy vector shape =
    case shape of
        BoundingBox2d box ->
            BoundingBox2d
                (BoundingBox2d.translateBy vector box)
