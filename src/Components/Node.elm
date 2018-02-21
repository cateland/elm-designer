module Node exposing (filterNode, getNode, updateNode)

import Components exposing (Component(Node))
import Entity exposing (Entity, addComponent, createEntity, getComponents)


getNode : Entity -> Maybe Component
getNode entity =
    case getComponents entity of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Node ->
                    Just x

                _ ->
                    getNode (createEntity xs)


filterNode : Entity -> Entity
filterNode entity =
    case getComponents entity of
        [] ->
            createEntity []

        x :: xs ->
            case x of
                Node ->
                    filterNode (createEntity xs)

                _ ->
                    addComponent x (filterNode (createEntity xs))


updateNode : Component -> Entity -> Entity
updateNode component entity =
    case component of
        Node ->
            case getNode entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterNode entity)

        _ ->
            entity
