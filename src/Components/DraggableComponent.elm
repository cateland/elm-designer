module DraggableComponent exposing (filterDraggable, getDraggable, updateDraggable)

import Components exposing (Component(DraggableComponent))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getDraggable : Entity -> Maybe Component
getDraggable entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                DraggableComponent _ ->
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
                DraggableComponent _ ->
                    filterDraggable (createEntity xs)

                _ ->
                    addComponent x (filterDraggable (createEntity xs))


updateDraggable : Component -> Entity -> Entity
updateDraggable component entity =
    case component of
        DraggableComponent _ ->
            case getDraggable entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterDraggable entity)

        _ ->
            entity