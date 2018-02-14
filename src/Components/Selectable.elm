module Selectable exposing (getSelectable, filterSelectable, updateSelectable)

import Components exposing (Entity(..), Component(Selectable), addComponent)


getSelectable : Entity -> Maybe Component
getSelectable (Entity components) =
    case components of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Selectable _ ->
                    Just x

                _ ->
                    getSelectable (Entity xs)


filterSelectable : Entity -> Entity
filterSelectable (Entity components) =
    case components of
        [] ->
            Entity components

        x :: xs ->
            case x of
                Selectable _ ->
                    filterSelectable (Entity xs)

                _ ->
                    addComponent x (filterSelectable (Entity xs))


updateSelectable : Component -> Entity -> Entity
updateSelectable component entity =
    case component of
        Selectable _ ->
            case getSelectable entity of
                Nothing ->
                    entity

                _ ->
                    addComponent component (filterSelectable entity)

        _ ->
            entity
