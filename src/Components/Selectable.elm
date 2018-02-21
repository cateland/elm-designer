module Selectable exposing (filterSelectable, getSelectable, updateSelectable)

import Components exposing (Component(Selectable))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getSelectable : Entity -> Maybe Component
getSelectable entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Selectable _ ->
                    Just x

                _ ->
                    getSelectable (createEntity xs)


filterSelectable : Entity -> Entity
filterSelectable entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Selectable _ ->
                    filterSelectable (createEntity xs)

                _ ->
                    addComponent x (filterSelectable (createEntity xs))


updateSelectable : Component -> Entity -> Entity
updateSelectable component entity =
    case component of
        Selectable _ ->
            case getSelectable entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterSelectable entity)

        _ ->
            entity
