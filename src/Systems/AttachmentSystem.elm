module AttachmentSystem exposing (..)

import Dict exposing (Dict)
import Components exposing (Entity, Component(Attachment, Node, Shape), Shape(..))
import Shape exposing (..)
import Attachment exposing (..)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Math exposing (Drag, isVectorOver, postionToPoint2d, translateBy, getCenterPosition)


findParentShape : String -> Dict String Entity -> Maybe Shape
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


applyAttachement : Dict String Entity -> Entity -> Entity
applyAttachement entities entity =
    case ( getAttachment entity, getShape entity ) of
        ( Just (Attachment parentId vector), Just (Shape shape) ) ->
            case findParentShape parentId entities of
                Just parentShape ->
                    let
                        actualVector =
                            Vector2d.from (getCenterPosition parentShape) (getCenterPosition shape)

                        newVector =
                            Vector2d.difference vector actualVector
                    in
                        updateShape
                            (Shape
                                (translateBy
                                    newVector
                                    shape
                                )
                            )
                            entity

                Nothing ->
                    entity

        _ ->
            entity
