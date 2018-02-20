module Draggable exposing (getDraggable, filterDraggable, updateDraggable)

import Entity exposing (Entity(..), addComponent)
import Components exposing (Component(Draggable))


getDraggable : Entity -> Maybe Component
getDraggable (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Draggable _ ->
                    Just x

                _ ->
                    getDraggable (Entity xs)


filterDraggable : Entity -> Entity
filterDraggable (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Draggable _ ->
                    filterDraggable (Entity xs)

                _ ->
                    addComponent x (filterDraggable (Entity xs))


updateDraggable : Component -> Entity -> Entity
updateDraggable component entity =
    case component of
        Draggable _ ->
            case getDraggable entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterDraggable entity)

        _ ->
            entity
