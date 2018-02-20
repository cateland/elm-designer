module Draggable exposing (getDraggable, filterDraggable, updateDraggable)

import Entity exposing (Entity, addComponent, getComponents, createEntity)
import Components exposing (Component(Draggable))


getDraggable : Entity -> Maybe Component
getDraggable entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Draggable _ ->
                    Just x

                _ ->
                    getDraggable (createEntity xs)


filterDraggable : Entity -> Entity
filterDraggable entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Draggable _ ->
                    filterDraggable (createEntity xs)

                _ ->
                    addComponent x (filterDraggable (createEntity xs))


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
