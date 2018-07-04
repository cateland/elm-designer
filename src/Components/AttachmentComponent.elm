module AttachmentComponent exposing (filterAttachment, getAttachment, updateAttachment, extractAttachment)

import Attachment exposing (Attachment)
import Components exposing (Component(AttachmentComponent))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


extractAttachment : Maybe Component -> Maybe Attachment
extractAttachment component =
    case component of
        Just (AttachmentComponent attachment) ->
            Just attachment

        _ ->
            Nothing


getAttachment : Entity -> Maybe Component
getAttachment entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                AttachmentComponent _ ->
                    Just x

                _ ->
                    getAttachment (createEntity xs)


filterAttachment : Entity -> Entity
filterAttachment entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                AttachmentComponent _ ->
                    filterAttachment (createEntity xs)

                _ ->
                    addComponent x (filterAttachment (createEntity xs))


updateAttachment : Component -> Entity -> Entity
updateAttachment component entity =
    case component of
        AttachmentComponent _ ->
            case getAttachment entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterAttachment entity)

        _ ->
            entity
