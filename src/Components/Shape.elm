module Shape exposing (..)

import Entity exposing (Entity, addComponent, getComponents, createEntity)
import Components exposing (Component(Shape))


getShape : Entity -> Maybe Component
getShape entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Shape _ ->
                    Just x

                _ ->
                    getShape (createEntity xs)


filterShape : Entity -> Entity
filterShape entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Shape _ ->
                    filterShape (createEntity xs)

                _ ->
                    addComponent x (filterShape (createEntity xs))


updateShape : Component -> Entity -> Entity
updateShape component entity =
    case component of
        Shape _ ->
            case getShape entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterShape entity)

        _ ->
            entity
