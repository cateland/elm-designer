module DragStatus exposing (getDragStatus, filterDragStatus, updateDragStatus)

import Entity exposing (Entity, addComponent, getComponents, createEntity)
import Components exposing (Component(DragStatus))


getDragStatus : Entity -> Maybe Component
getDragStatus entity=
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                DragStatus _ ->
                    Just x

                _ ->
                    getDragStatus (createEntity xs)


filterDragStatus : Entity -> Entity
filterDragStatus entity=
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                DragStatus _ ->
                    filterDragStatus (createEntity xs)

                _ ->
                    addComponent x (filterDragStatus (createEntity xs))


updateDragStatus : Component -> Entity -> Entity
updateDragStatus component entity =
    case component of
        DragStatus _ ->
            case getDragStatus entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterDragStatus entity)

        _ ->
            entity
