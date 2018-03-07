module Math exposing (..)

import Components exposing (Shape(..))
import Mouse exposing (Position)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (translateBy)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


positionToPoint2d : Position -> Point2d
positionToPoint2d position =
    Point2d.fromCoordinates ( toFloat position.x, toFloat position.y )


isVectorOver : Point2d -> Components.Shape -> Bool
isVectorOver point2d shape =
    case
        shape
    of
        BoundingBox2d box ->
            BoundingBox2d.contains point2d box

        Circle2d circle ->
            Circle2d.contains point2d circle

        LineSegment2d _ ->
            False


translateBy : Vector2d -> Components.Shape -> Components.Shape
translateBy vector shape =
    case shape of
        BoundingBox2d box ->
            BoundingBox2d
                (BoundingBox2d.translateBy vector box)

        Circle2d circle ->
            Circle2d (Circle2d.translateBy vector circle)

        LineSegment2d lineSegment ->
            LineSegment2d (LineSegment2d.translateBy vector lineSegment)


getCenterPosition : Components.Shape -> Point2d
getCenterPosition shape =
    case shape of
        BoundingBox2d box ->
            BoundingBox2d.centroid box

        Circle2d circle ->
            Circle2d.centerPoint circle

        LineSegment2d lineSegment ->
            LineSegment2d.midpoint lineSegment


getShapeBoundingBox : Components.Shape -> BoundingBox2d.BoundingBox2d
getShapeBoundingBox shape =
    case shape of
        BoundingBox2d box ->
            box

        Circle2d circle ->
            Circle2d.boundingBox circle

        LineSegment2d segment ->
            LineSegment2d.boundingBox segment
