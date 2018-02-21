module Brush exposing (filterBrush, getBrush, updateBrush)

import Components exposing (Component(Brush))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getBrush : Entity -> Maybe Component
getBrush entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Brush _ ->
                    Just x

                _ ->
                    getBrush (createEntity xs)


filterBrush : Entity -> Entity
filterBrush entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Brush _ ->
                    filterBrush (createEntity xs)

                _ ->
                    addComponent x (filterBrush (createEntity xs))


updateBrush : Component -> Entity -> Entity
updateBrush component entity =
    case component of
        Brush _ ->
            case getBrush entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterBrush entity)

        _ ->
            entity
