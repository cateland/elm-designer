module DragStatus exposing (getDragStatus, filterDragStatus, updateDragStatus)

import Components exposing (Entity(..), Component(DragStatus), addComponent)


getDragStatus : Entity -> Maybe Component
getDragStatus (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                DragStatus _ ->
                    Just x

                _ ->
                    getDragStatus (Entity xs)


filterDragStatus : Entity -> Entity
filterDragStatus (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                DragStatus _ ->
                    filterDragStatus (Entity xs)

                _ ->
                    addComponent x (filterDragStatus (Entity xs))


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
