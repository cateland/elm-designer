module Brush exposing (getBrush, filterBrush, updateBrush)

import Entity exposing (Entity, addComponent, getComponents, createEntity)
import Components exposing (Component(Brush))


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
