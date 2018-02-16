module Drawable exposing (getDrawable, filterDrawable, updateDrawable)

import Components exposing (Entity(..), Component(Drawable), addComponent)


getDrawable : Entity -> Maybe Component
getDrawable (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Drawable _ ->
                    Just x

                _ ->
                    getDrawable (Entity xs)


filterDrawable : Entity -> Entity
filterDrawable (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Drawable _ ->
                    filterDrawable (Entity xs)

                _ ->
                    addComponent x (filterDrawable (Entity xs))


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
