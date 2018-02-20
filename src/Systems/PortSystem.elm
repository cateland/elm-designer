module PortSystem exposing (..)

import Dict exposing (Dict)
import Entity exposing (Entities, Entity, addComponent)
import Components
    exposing
        ( Component(Attachment, Port, Node, Shape)
        , Port(..)
        , Shape(..)
        , Drag
        )
import Shape exposing (..)
import Port exposing (..)
import Node exposing (..)
import Attachment exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Math exposing ( isVectorOver, postionToPoint2d, translateBy, getCenterPosition)


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


applyPort : Entities -> Entity -> Entity
applyPort entities entity =
    case ( getPort entity, getShape entity ) of
        ( Just (Port (PortSource nodeId)), Just (Shape portShape) ) ->
            case findNodeShape nodeId entities of
                Just nodeShape ->
                    case getAttachment entity of
                        Just (Attachment _ _) ->
                            entity

                        _ ->
                            addComponent (Attachment nodeId (calculatePortAttachement getSourcePortPosition nodeShape)) entity

                Nothing ->
                    entity

        ( Just (Port (PortSink nodeId)), Just (Shape portShape) ) ->
            case findNodeShape nodeId entities of
                Just nodeShape ->
                    case getAttachment entity of
                        Just (Attachment _ _) ->
                            entity

                        _ ->
                            addComponent (Attachment nodeId (calculatePortAttachement getSinkPortPosition nodeShape)) entity

                Nothing ->
                    entity

        _ ->
            entity
