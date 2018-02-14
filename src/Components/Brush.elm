module Brush exposing (getBrush, filterBrush, updateBrush)

import Components exposing (Entity(..), Component(Brush), addComponent)


getBrush : Entity -> Maybe Component
getBrush (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Brush _ _ ->
                    Just x

                _ ->
                    getBrush (Entity xs)


filterBrush : Entity -> Entity
filterBrush (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Brush _ _ ->
                    filterBrush (Entity xs)

                _ ->
                    addComponent x (filterBrush (Entity xs))


updateBrush : Component -> Entity -> Entity
updateBrush component entity =
    case component of
        Brush _ _ ->
            case getBrush entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterBrush entity)

        _ ->
            entity
