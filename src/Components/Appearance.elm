module Appearance exposing (getAppearance, filterAppearance, updateAppearance)

import Entity exposing (Entity, addComponent, getComponents, createEntity)
import Components exposing (Component(Appearance))


getAppearance : Entity -> Maybe Component
getAppearance entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Appearance _ ->
                    Just x

                _ ->
                    getAppearance (createEntity xs)


filterAppearance : Entity -> Entity
filterAppearance entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Appearance _ ->
                    filterAppearance (createEntity xs)

                _ ->
                    addComponent x (filterAppearance (createEntity xs))


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
