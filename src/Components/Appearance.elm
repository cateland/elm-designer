module Appearance exposing (getAppearance, filterAppearance, updateAppearance)

import Entity exposing (Entity(..), addComponent)
import Components exposing (Component(Appearance))


getAppearance : Entity -> Maybe Component
getAppearance (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Appearance _ ->
                    Just x

                _ ->
                    getAppearance (Entity xs)


filterAppearance : Entity -> Entity
filterAppearance (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Appearance _ ->
                    filterAppearance (Entity xs)

                _ ->
                    addComponent x (filterAppearance (Entity xs))


updateAppearance : Component -> Entity -> Entity
updateAppearance component entity =
    case component of
        Appearance _ ->
            case getAppearance entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterAppearance entity)

        _ ->
            entity
