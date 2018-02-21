module MultiSelectDragSystem exposing (..)

import Components
    exposing
        ( Component(DraggableComponent, Drawable, Selectable, Shape)
        , Shape(BoundingBox2d)
        )
import Dict
import Draggable exposing (createDragged, toggleDraggable)
import Entity exposing (Entities, Entity, createEntity)
import Math exposing (getShapeBoundingBox)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Selectable exposing (getSelectable)
import Shape exposing (getShape)


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
    case Dict.size selectedEntities > 1 of
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
                                    hull
                                )
                            , DraggableComponent createDragged
                            ]
                        )
                        entities

                _ ->
                    entities

        False ->
            Dict.remove "multiDrag" entities
