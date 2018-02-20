module Hoverable exposing (getHoverable, filterHoverable, updateHoverable)

import Entity exposing (Entity(..), addComponent)
import Components exposing (Component(Hoverable))


getHoverable : Entity -> Maybe Component
getHoverable (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Hoverable _ ->
                    Just x

                _ ->
                    getHoverable (Entity xs)


filterHoverable : Entity -> Entity
filterHoverable (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Hoverable _ ->
                    filterHoverable (Entity xs)

                _ ->
                    addComponent x (filterHoverable (Entity xs))


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
