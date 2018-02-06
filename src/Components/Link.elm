module Link exposing (getLink, filterLink, updateLink)

import Components exposing (Entity(..), Component(Link), addComponent)


getLink : Entity -> Maybe Component
getLink (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Link _ _ ->
                    Just x

                _ ->
                    getLink (Entity xs)


filterLink : Entity -> Entity
filterLink (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Link _ _ ->
                    filterLink (Entity xs)

                _ ->
                    addComponent x (filterLink (Entity xs))


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
