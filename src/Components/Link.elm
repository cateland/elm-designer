module Link exposing (getLink, filterLink, updateLink)

import Entity exposing (Entity, addComponent, getComponents, createEntity)
import Components exposing (Component(Link))


getLink : Entity -> Maybe Component
getLink entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Link _ _ ->
                    Just x

                _ ->
                    getLink (createEntity xs)


filterLink : Entity -> Entity
filterLink entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Link _ _ ->
                    filterLink (createEntity xs)

                _ ->
                    addComponent x (filterLink (createEntity xs))


updateLink : Component -> Entity -> Entity
updateLink component entity =
    case component of
        Link _ _ ->
            case getLink entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterLink entity)

        _ ->
            entity
