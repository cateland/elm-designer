module Node exposing (getNode, filterNode, updateNode)

import Components exposing (Entity(..), Component(Node), addComponent)


getNode : Entity -> Maybe Component
getNode (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Node ->
                    Just x

                _ ->
                    getNode (Entity xs)


filterNode : Entity -> Entity
filterNode (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Node ->
                    filterNode (Entity xs)

                _ ->
                    addComponent x (filterNode (Entity xs))


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
