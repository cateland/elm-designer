module Attachment exposing (filterAttachment, getAttachment, updateAttachment)

import Components exposing (Component(Attachment))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getAttachment : Entity -> Maybe Component
getAttachment entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Attachment _ _ ->
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
                Attachment _ _ ->
                    filterAttachment (createEntity xs)

                _ ->
                    addComponent x (filterAttachment (createEntity xs))


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
