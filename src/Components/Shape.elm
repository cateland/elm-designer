module Shape exposing (..)

import Components exposing (Entity(..), Component(Shape), addComponent)


getShape : Entity -> Maybe Component
getShape (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Shape _ ->
                    Just x

                _ ->
                    getShape (Entity xs)


filterShape : Entity -> Entity
filterShape (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Shape _ ->
                    filterShape (Entity xs)

                _ ->
                    addComponent x (filterShape (Entity xs))


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
