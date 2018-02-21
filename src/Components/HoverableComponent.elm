module HoverableComponent exposing (filterHoverable, getHoverable, updateHoverable)

import Components exposing (Component(HoverableComponent))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getHoverable : Entity -> Maybe Component
getHoverable entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                HoverableComponent _ ->
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
                HoverableComponent _ ->
                    filterHoverable (createEntity xs)

                _ ->
                    addComponent x (filterHoverable (createEntity xs))


updateHoverable : Component -> Entity -> Entity
updateHoverable component entity =
    case component of
        HoverableComponent _ ->
            case getHoverable entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterHoverable entity)

        _ ->
            entity
