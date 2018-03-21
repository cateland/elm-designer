module PortSystem exposing (portSystem)

import Attachment exposing (..)
import Components
    exposing
        ( Component(Attachment, Node, Port, Shape)
        , Drag
        , Port(..)
        )
import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities, addComponent)
import Msgs exposing (Msg)
import Node exposing (..)
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Port exposing (..)
import Shape exposing (Shape(..), getCenterPosition, isVectorOver, translateBy)
import ShapeComponent exposing (getShape)


findNodeShape : String -> Entities -> Maybe Shape
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
            Arc2d.pointOn (Circle2d.toArc circle) 0.5

        LineSegment2d lineSegment ->
            LineSegment2d.startPoint lineSegment


getSourcePortPosition : Shape -> Point2d
getSourcePortPosition shape =
    case shape of
        BoundingBox2d box ->
            Point2d.fromCoordinates ( BoundingBox2d.maxX box, BoundingBox2d.midY box )

        Circle2d circle ->
            Arc2d.pointOn (Circle2d.toArc circle) 0

        LineSegment2d lineSegment ->
            LineSegment2d.endPoint lineSegment


calculatePortAttachement :
    (Shape -> Point2d)
    -> Shape
    -> Vector2d
calculatePortAttachement getPosition nodeShape =
    nodeShape
        |> getPosition
        |> Vector2d.from (getCenterPosition nodeShape)


portSystem : Msgs.Msg -> Entities -> String -> (Entity, NewEntities) -> (Entity, NewEntities)
portSystem msg entities key (entity, newEntities) =
    case ( getPort entity, getShape entity ) of
        ( Just (Port (PortSource nodeId)), Just (Shape portShape) ) ->
            case findNodeShape nodeId entities of
                Just nodeShape ->
                    case getAttachment entity of
                        Just (Attachment _ _) ->
                            (entity, newEntities)

                        _ ->
                            (addComponent (Attachment nodeId (calculatePortAttachement getSourcePortPosition nodeShape)) entity, newEntities)

                Nothing ->
                    (entity, newEntities)

        ( Just (Port (PortSink nodeId)), Just (Shape portShape) ) ->
            case findNodeShape nodeId entities of
                Just nodeShape ->
                    case getAttachment entity of
                        Just (Attachment _ _) ->
                            (entity, newEntities)

                        _ ->
                            (addComponent (Attachment nodeId (calculatePortAttachement getSinkPortPosition nodeShape)) entity, newEntities)

                Nothing ->
                    (entity, newEntities)

        _ ->
            (entity, newEntities)
