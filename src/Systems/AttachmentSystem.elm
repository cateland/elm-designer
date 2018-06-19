module AttachmentSystem exposing (..)

import Attachment exposing (..)
import Components
    exposing
        ( Component(Attachment, Node, Shape)
        , Drag
        )
import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities)
import Msgs exposing (Msg)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Shape exposing (Shape, getCenterPosition, isVectorOver, translateBy)
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

        Nothing ->
            Nothing

attachementSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
attachementSystem msg entities key ( entity, newEntities ) =
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
                    ( updateShape
                        (Shape
                            (translateBy
                                newVector
                                shape
                            )
                        )
                        entity
                    , newEntities
                    )

                Nothing ->
                    ( entity, newEntities )

        _ ->
            ( entity, newEntities )


newAttachementSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
newAttachementSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
    case getAttachment entity of
        Just (Attachment parentId vector) ->
            case getShape entity of
                Just (Shape shape) ->
                    case findParentShape parentId entities of
                        Just parentShape ->
                            let
                                actualVector =
                                    Vector2d.from (getCenterPosition parentShape) (getCenterPosition shape)

                                newVector =
                                    Vector2d.difference vector actualVector
                            in
                            ( updateShape
                                (Shape
                                    (translateBy
                                        newVector
                                        shape
                                    )
                                )
                                entity
                            , newEntities
                            )

                        Nothing ->
                            tuple

                _ ->
                    tuple

        _ ->
            tuple
