module Attachment exposing (getAttachment, filterAttachment, updateAttachment)

import Entity exposing (Entity(..), addComponent)
import Components exposing (Component(Attachment))


getAttachment : Entity -> Maybe Component
getAttachment (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Attachment _ _ ->
                    Just x

                _ ->
                    getAttachment (Entity xs)


filterAttachment : Entity -> Entity
filterAttachment (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Attachment _ _ ->
                    filterAttachment (Entity xs)

                _ ->
                    addComponent x (filterAttachment (Entity xs))


updateAttachment : Component -> Entity -> Entity
updateAttachment component entity =
    case component of
        Attachment _ _ ->
            case getAttachment entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterAttachment entity)

        _ ->
            entity
