module PortSystem exposing (..)

import Dict exposing (Dict)
import Components exposing (Entity, Component(Port, Node, Shape), Port(..), Shape(..))
import Shape exposing (..)
import Port exposing (..)
import Node exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Math exposing (Drag, isVectorOver, postionToPoint2d, translateBy, getCenterPosition)


findNodeShape : String -> Dict String Entity -> Maybe Shape
findNodeShape key entities =
    case Dict.get key entities of
        Just entity ->
            case ( getShape entity, getNode entity ) of
                ( Just (Shape nodeShape), Just Node ) ->
                    Just nodeShape

                _ ->
                    Nothing

        _ ->
            Nothing


getSinkPortPosition : Shape -> Point2d
getSinkPortPosition shape =
    case shape of
        BoundingBox2d box ->
            Point2d.fromCoordinates ( BoundingBox2d.minX box, BoundingBox2d.midY box )

        Circle2d circle ->
            Arc2d.pointOn (Circle2d.toArc circle) 0


getSourcePortPosition : Shape -> Point2d
getSourcePortPosition shape =
    case shape of
        BoundingBox2d box ->
            Point2d.fromCoordinates ( BoundingBox2d.maxX box, BoundingBox2d.midY box )

        Circle2d circle ->
            Arc2d.pointOn (Circle2d.toArc circle) 0.5



-- i don't like this one...


translatePort :
    (a -> Point2d)
    -> Shape
    -> a
    -> Entity
    -> Entity
translatePort getPosition portShape nodeShape entity =
    portShape
        |> getCenterPosition
        |> flip (Vector2d.from) (getPosition nodeShape)
        |> flip (translateBy) portShape
        |> Shape
        |> flip (updateShape) entity


applyPort : Dict String Entity -> Entity -> Entity
applyPort entities entity =
    case ( getPort entity, getShape entity ) of
        ( Just (Port (PortSource nodeId)), Just (Shape portShape) ) ->
            case findNodeShape nodeId entities of
                Just nodeShape ->
                    translatePort getSourcePortPosition portShape nodeShape entity

                Nothing ->
                    entity

        ( Just (Port (PortSink nodeId)), Just (Shape portShape) ) ->
            case findNodeShape nodeId entities of
                Just nodeShape ->
                    translatePort getSinkPortPosition portShape nodeShape entity

                Nothing ->
                    entity

        _ ->
            entity
