module LinkSystem exposing (..)

import Dict exposing (Dict)
import Components
    exposing
        ( Entity
        , Component(Attachment, Port, Node, Link, Shape)
        , Port(..)
        , Shape(..)
        , addComponent
        )
import Shape exposing (..)
import Link exposing (..)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Math exposing (Drag, isVectorOver, postionToPoint2d, translateBy, getCenterPosition)


findParentShape : String -> Dict String Entity -> Maybe Shape
findParentShape key entities =
    case Dict.get key entities of
        Just entity ->
            case (getShape entity) of
                Just (Shape nodeShape) ->
                    Just nodeShape

                _ ->
                    Nothing

        _ ->
            Nothing


applyLink : Dict String Entity -> Entity -> Entity
applyLink entities entity =
    case (getLink entity) of
        Just (Link sourceId targetId) ->
            case ( findParentShape sourceId entities, findParentShape targetId entities ) of
                ( Just sourceShape, Just targetShape ) ->
                    case (getShape entity) of
                        Just shape ->
                            updateShape
                                (Shape
                                    (LineSegment2d (LineSegment2d.fromEndpoints ( (getCenterPosition sourceShape), (getCenterPosition targetShape) )))
                                )
                                entity

                        Nothing ->
                            addComponent (Shape (LineSegment2d (LineSegment2d.fromEndpoints ( (getCenterPosition sourceShape), (getCenterPosition targetShape) )))) entity

                _ ->
                    entity

        _ ->
            entity