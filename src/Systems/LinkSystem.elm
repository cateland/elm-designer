module LinkSystem exposing (linkSystem)

import Components
    exposing
        ( Component(Attachment, Link, Node, Port, Shape)
        , Drag
        , Port
        , Shape(..)
        )
import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities, addComponent)
import Link exposing (..)
import Math exposing (getCenterPosition, isVectorOver, postionToPoint2d, translateBy)
import Msgs exposing (Msg)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Shape exposing (..)


findParentShape : String -> Entities -> Maybe Shape
findParentShape key entities =
    case Dict.get key entities of
        Just entity ->
            case getShape entity of
                Just (Shape nodeShape) ->
                    Just nodeShape

                _ ->
                    Nothing

        _ ->
            Nothing


linkSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
linkSystem msg entities key ( entity, newEntities ) =
    case getLink entity of
        Just (Link sourceId targetId) ->
            case ( findParentShape sourceId entities, findParentShape targetId entities ) of
                ( Just sourceShape, Just targetShape ) ->
                    case getShape entity of
                        Just shape ->
                            ( updateShape
                                (Shape
                                    (LineSegment2d (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape )))
                                )
                                entity
                            , newEntities
                            )

                        Nothing ->
                            ( addComponent (Shape (LineSegment2d (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape )))) entity, newEntities )

                _ ->
                    ( entity, newEntities )

        _ ->
            ( entity, newEntities )
