module Link exposing (filterLink, getLink, updateLink)

import Components exposing (Component(Link))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


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
