module AttachmentSystem exposing (..)

import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities)
import Msgs exposing (Msg)
import Vector2d as Vector2d exposing (Vector2d)
import Shape exposing (Shape, getCenterPosition, isVectorOver, translateBy)
import Attachment exposing (getAttachmentTarget, getAttachmentOffset)


findParentShape : String -> Entities -> Maybe Shape
findParentShape key entities =
    Dict.get key entities |> Maybe.andThen .shape


updatePosition : Shape -> Shape -> Vector2d -> Entity -> Entity
updatePosition shape parentShape vector entity =
    let
        actualVector =
            Vector2d.from (getCenterPosition parentShape) (getCenterPosition shape)

        newVector =
            Vector2d.difference vector actualVector
    in
        { entity
            | shape =
                Just
                    (translateBy
                        newVector
                        shape
                    )
        }


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


attachementSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
attachementSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        case entity |> .attachment of
            Just attachment ->
                case entity |> .shape of
                    Just shape ->
                        whatever attachment shape entities entity newEntities tuple

                    Nothing ->
                        tuple

            Nothing ->
                tuple
