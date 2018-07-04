module PortSystem exposing (portSystem)

import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities)
import Msgs exposing (Msg)
import Arc2d as Arc2d exposing (Arc2d)
import BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Circle2d as Circle2d exposing (Circle2d)
import LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Point2d as Point2d exposing (Point2d)
import Vector2d as Vector2d exposing (Vector2d)
import Curve.ParameterValue as ParameterValue
import Port exposing (..)
import Shape exposing (Shape(..), getCenterPosition, isVectorOver, translateBy)
import Attachment exposing (Attachment, createAttachment)


findNodeShape : String -> Entities -> Maybe Shape
findNodeShape key entities =
    case Dict.get key entities of
        Just entity ->
            case (.shape entity) of
                Just nodeShape ->
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
            Arc2d.pointOn (Circle2d.toArc circle) ParameterValue.half

        LineSegment2d lineSegment ->
            LineSegment2d.startPoint lineSegment


getSourcePortPosition : Shape -> Point2d
getSourcePortPosition shape =
    case shape of
        BoundingBox2d box ->
            Point2d.fromCoordinates ( BoundingBox2d.maxX box, BoundingBox2d.midY box )

        Circle2d circle ->
            Arc2d.pointOn (Circle2d.toArc circle) ParameterValue.zero

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


portSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
portSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        case .portComponent entity of
            Just portComponent ->
                case isPortSource portComponent of
                    True ->
                        case .attachment entity of
                            Just attachment ->
                                tuple

                            _ ->
                                case .shape entity of
                                    Just portShape ->
                                        case findNodeShape (getTarget portComponent) entities of
                                            Just nodeShape ->
                                                ( { entity | attachment = Just (createAttachment (getTarget portComponent) (calculatePortAttachement getSourcePortPosition nodeShape)) }, newEntities )

                                            Nothing ->
                                                tuple

                                    _ ->
                                        tuple

                    False ->
                        case .attachment entity of
                            Just _ ->
                                tuple

                            _ ->
                                case .shape entity of
                                    Just portShape ->
                                        case findNodeShape (getTarget portComponent) entities of
                                            Just nodeShape ->
                                                case .attachment entity of
                                                    Just _ ->
                                                        tuple

                                                    _ ->
                                                        ( { entity | attachment = Just (createAttachment (getTarget portComponent) (calculatePortAttachement getSinkPortPosition nodeShape)) }, newEntities )

                                            Nothing ->
                                                tuple

                                    _ ->
                                        tuple

            _ ->
                tuple
