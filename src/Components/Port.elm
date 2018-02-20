module Port exposing (getPort, filterPort, updatePort)

import Entity exposing (Entity(..), addComponent)
import Components exposing (Component(Port))


getPort : Entity -> Maybe Component
getPort (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Port _ ->
                    Just x

                _ ->
                    getPort (Entity xs)


filterPort : Entity -> Entity
filterPort (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Port _ ->
                    filterPort (Entity xs)

                _ ->
                    addComponent x (filterPort (Entity xs))


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
