module Port exposing (getPort, filterPort, updatePort)

import Entity exposing (Entity, addComponent, getComponents, createEntity)
import Components exposing (Component(Port))


getPort : Entity -> Maybe Component
getPort entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Port _ ->
                    Just x

                _ ->
                    getPort (createEntity xs)


filterPort : Entity -> Entity
filterPort entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Port _ ->
                    filterPort (createEntity xs)

                _ ->
                    addComponent x (filterPort (createEntity xs))


updatePort : Component -> Entity -> Entity
updatePort component entity =
    case component of
        Port _ ->
            case getPort entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterPort entity)

        _ ->
            entity
