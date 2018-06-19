module LinkSystem exposing (linkSystem, newLinkSystem)

import Components
    exposing
        ( Component(Attachment, Link, Node, Port, Shape)
        , Drag
        , Port
        )
import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities, addComponent)
import Link exposing (..)
import Msgs exposing (Msg)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Shape exposing (Shape, createLineSegment, getCenterPosition, isVectorOver, translateBy)
import ShapeComponent exposing (getShape, updateShape)


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
linkSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
    case getLink entity of
        Just (Link sourceId targetId) ->
            case ( findParentShape sourceId entities, findParentShape targetId entities ) of
                ( Just sourceShape, Just targetShape ) ->
                    case getShape entity of
                        Just shape ->
                            ( updateShape
                                (Shape
                                    (createLineSegment (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape )))
                                )
                                entity
                            , newEntities
                            )

                        Nothing ->
                            ( addComponent (Shape (createLineSegment (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape )))) entity, newEntities )

                _ ->
                    tuple

        _ ->
            tuple


newLinkSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
newLinkSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
    case getLink entity of
        Just (Link sourceId targetId) ->
            case ( findParentShape sourceId entities, findParentShape targetId entities ) of
                ( Just sourceShape, Just targetShape ) ->
                    case getShape entity of
                        Just shape ->
                            ( updateShape
                                (Shape
                                    (createLineSegment (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape )))
                                )
                                entity
                            , newEntities
                            )

                        Nothing ->
                            ( addComponent (Shape (createLineSegment (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape )))) entity, newEntities )

                _ ->
                    tuple

        _ ->
            tuple
