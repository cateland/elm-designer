module AttachmentSystem exposing (..)

import Attachment exposing (..)
import Components
    exposing
        ( Component(Attachment, Node, Shape)
        , Drag
        , Shape
        )
import Dict exposing (Dict)
import Entity exposing (Entities, Entity)
import Math exposing (getCenterPosition, isVectorOver, postionToPoint2d, translateBy)
import Msgs exposing (Msg)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
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


attachementSystem : Msgs.Msg -> Entities -> String -> Entity -> Entity
attachementSystem msg entities key entity =
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
