module AttachmentSystem exposing (..)

import Components
    exposing
        ( Component(Node, ShapeComponent)
        , Drag
        )
import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities)
import Msgs exposing (Msg)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Shape exposing (Shape, getCenterPosition, isVectorOver, translateBy)
import ShapeComponent exposing (getShape, extractShape, updateShape)
import Attachment exposing (getAttachmentTarget, getAttachmentOffset)
import AttachmentComponent exposing (getAttachment, extractAttachment)


findParentShape : String -> Entities -> Maybe Shape
findParentShape key entities =
    Dict.get key entities |> Maybe.andThen getShape |> extractShape


attachementSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
attachementSystem msg entities key ( entity, newEntities ) =
    case ( entity |> getAttachment |> extractAttachment, getShape entity ) of
        ( Just attachment, Just (ShapeComponent shape) ) ->
            case findParentShape (getAttachmentTarget attachment) entities of
                Just parentShape ->
                    let
                        actualVector =
                            Vector2d.from (getCenterPosition parentShape) (getCenterPosition shape)

                        newVector =
                            Vector2d.difference (getAttachmentOffset attachment) actualVector
                    in
                        ( updateShape
                            (ShapeComponent
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


updatePosition : Shape -> Shape -> Vector2d -> Entity -> Entity
updatePosition shape parentShape vector entity =
    let
        actualVector =
            Vector2d.from (getCenterPosition parentShape) (getCenterPosition shape)

        newVector =
            Vector2d.difference vector actualVector
    in
        updateShape
            (ShapeComponent
                (translateBy
                    newVector
                    shape
                )
            )
            entity


whatever :
    Attachment.Attachment
    -> Shape
    -> Entities
    -> Entity
    -> a
    -> ( Entity, a )
    -> ( Entity, a )
whatever attachment shape entities entity newEntities tuple =
    case findParentShape (getAttachmentTarget attachment) entities of
        Just parentShape ->
            let
                newEntity =
                    updatePosition shape parentShape (getAttachmentOffset attachment) entity
            in
                ( newEntity
                , newEntities
                )

        Nothing ->
            tuple


newAttachementSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
newAttachementSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        case entity |> getAttachment |> extractAttachment of
            Just attachment ->
                case entity |> getShape |> extractShape of
                    Just shape ->
                        whatever attachment shape entities entity newEntities tuple

                    Nothing ->
                        tuple

            Nothing ->
                tuple
