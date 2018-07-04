module Shape
    exposing
        ( Shape(BoundingBox2d, Circle2d, LineSegment2d)
        , createBoundingBox
        , createLineSegment
        , getCenterPosition
        , getShapeBoundingBox
        , isVectorOver
        , translateBy
        )

import BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Circle2d as Circle2d exposing (Circle2d)
import LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Point2d as Point2d exposing (Point2d)
import Vector2d as Vector2d exposing (Vector2d)


type Shape
    = BoundingBox2d BoundingBox2d
    | Circle2d Circle2d
    | LineSegment2d LineSegment2d

    

createLineSegment : LineSegment2d -> Shape
createLineSegment lineSegment =
    LineSegment2d lineSegment


createBoundingBox : BoundingBox2d -> Shape
createBoundingBox boundingBox =
    BoundingBox2d boundingBox


isVectorOver : Point2d -> Shape -> Bool
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


translateBy : Vector2d -> Shape -> Shape
translateBy vector shape =
    case shape of
        BoundingBox2d box ->
            BoundingBox2d
                (BoundingBox2d.translateBy vector box)

        Circle2d circle ->
            Circle2d (Circle2d.translateBy vector circle)

        LineSegment2d lineSegment ->
            LineSegment2d (LineSegment2d.translateBy vector lineSegment)


getCenterPosition : Shape -> Point2d
getCenterPosition shape =
    case shape of
        BoundingBox2d box ->
            BoundingBox2d.centroid box

        Circle2d circle ->
            Circle2d.centerPoint circle

        LineSegment2d lineSegment ->
            LineSegment2d.midpoint lineSegment


getShapeBoundingBox : Shape -> BoundingBox2d.BoundingBox2d
getShapeBoundingBox shape =
    case shape of
        BoundingBox2d box ->
            box

        Circle2d circle ->
            Circle2d.boundingBox circle

        LineSegment2d segment ->
            LineSegment2d.boundingBox segment
