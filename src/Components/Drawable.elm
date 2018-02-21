module Drawable exposing (filterDrawable, getDrawable, updateDrawable)

import Components exposing (Component(Drawable))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getDrawable : Entity -> Maybe Component
getDrawable entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Drawable _ ->
                    Just x

                _ ->
                    getDrawable (createEntity xs)


filterDrawable : Entity -> Entity
filterDrawable entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Drawable _ ->
                    filterDrawable (createEntity xs)

                _ ->
                    addComponent x (filterDrawable (createEntity xs))


updateDrawable : Component -> Entity -> Entity
updateDrawable component entity =
    case component of
        Drawable _ ->
            case getDrawable entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterDrawable entity)

        _ ->
            entity
