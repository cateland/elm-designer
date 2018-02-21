module Hoverable exposing (filterHoverable, getHoverable, updateHoverable)

import Components exposing (Component(Hoverable))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getHoverable : Entity -> Maybe Component
getHoverable entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Hoverable _ ->
                    Just x

                _ ->
                    getHoverable (createEntity xs)


filterHoverable : Entity -> Entity
filterHoverable entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Hoverable _ ->
                    filterHoverable (createEntity xs)

                _ ->
                    addComponent x (filterHoverable (createEntity xs))


updateHoverable : Component -> Entity -> Entity
updateHoverable component entity =
    case component of
        Hoverable _ ->
            case getHoverable entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterHoverable entity)

        _ ->
            entity
