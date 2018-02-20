module MultiSelectDragSystem exposing (..)

import Math exposing (getShapeBoundingBox)
import Entity exposing (Entities, Entity, createEntity)
import Components
    exposing
        ( Component(Selectable, Shape, DraggableComponent, Drawable)
        , Shape(BoundingBox2d)
        )
import Draggable exposing (createDragged, toggleDraggable)
import Selectable exposing (getSelectable)
import Shape exposing (getShape)
import Dict
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)


filter : String -> Entity -> Bool
filter key entity =
    case ( getSelectable entity, getShape entity ) of
        ( Just (Selectable (Components.Selected _)), Just (Shape entityShape) ) ->
            True

        _ ->
            False


filterSelectedEntities : Entities -> Entities
filterSelectedEntities entities =
    Dict.filter filter entities


extractShape : Entity -> Maybe Component
extractShape entity =
    getShape entity


foldIntoBox : Maybe Component -> Maybe BoundingBox2d
foldIntoBox shape =
    case shape of
        Just (Shape shape) ->
            Just (getShapeBoundingBox shape)

        _ ->
            Nothing


convertShapeListToBox : Entities -> Maybe BoundingBox2d
convertShapeListToBox entities =
    BoundingBox2d.hullOf (List.filterMap (foldIntoBox << extractShape) (Dict.values entities))


applyMultiSelectDrag : Entities -> Entities
applyMultiSelectDrag entities =
    let
        selectedEntities =
            filterSelectedEntities entities
    in
        case (Dict.size selectedEntities) > 1 of
            True ->
                let
                    boxList =
                        selectedEntities
                in
                    case convertShapeListToBox boxList of
                        Just hull ->
                            Dict.insert "multiDrag"
                                (createEntity
                                    [ Drawable 60
                                    , Shape
                                        (BoundingBox2d
                                            (hull)
                                        )
                                    , DraggableComponent createDragged
                                    ]
                                )
                                entities

                        _ ->
                            entities

            False ->
                Dict.remove "multiDrag" entities
