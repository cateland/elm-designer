module MultiSelectDragSystem exposing (multiSelectDragSystem, newMultiSelectDragSystem)

import Attribute exposing (fill)
import Components
    exposing
        ( Component(DraggableComponent, Drawable, HoverableComponent, SelectableComponent, ShapeComponent)
        )
import Dict
import Draggable exposing (createDragged, toggleDraggable)
import Entity exposing (Entities, Entity, NewEntities, addToNewEntitiesWithKey, createEntity)
import Hoverable exposing (createNotHovered)
import Msgs exposing (Msg)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Selectable exposing (getSelectable)
import Shape exposing (getShapeBoundingBox, createBoundingBox)
import ShapeComponent exposing (getShape)


filter : String -> Entity -> Bool
filter key entity =
    case ( getSelectable entity, getShape entity ) of
        ( Just (SelectableComponent (Components.Selected _)), Just (ShapeComponent entityShape) ) ->
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
        Just (ShapeComponent shape) ->
            Just (getShapeBoundingBox shape)

        _ ->
            Nothing


convertShapeListToBox : Entities -> Maybe BoundingBox2d
convertShapeListToBox entities =
    BoundingBox2d.hullOf (List.filterMap (foldIntoBox << extractShape) (Dict.values entities))


multiSelectDragSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
multiSelectDragSystem msg entities key tuple =
    let
        ( entity, newEntities ) = tuple
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
                    ( entity
                    , addToNewEntitiesWithKey "multiselectDrag"
                        (createEntity
                            [ Drawable 60
                            , ShapeComponent
                                (createBoundingBox
                                    hull
                                )
                            , DraggableComponent createDragged
                            , HoverableComponent
                                (createNotHovered
                                    [ fill "red" ]
                                )
                            ]
                        )
                        newEntities
                    )

                _ ->
                    tuple

        False ->
            -- Dict.remove "multiDrag" entities
            tuple

newMultiSelectDragSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
newMultiSelectDragSystem msg entities key tuple =
    let
        ( entity, newEntities ) = tuple
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
                    ( entity
                    , addToNewEntitiesWithKey "multiselectDrag"
                        (createEntity
                            [ Drawable 60
                            , ShapeComponent
                                (createBoundingBox
                                    hull
                                )
                            , DraggableComponent createDragged
                            , HoverableComponent
                                (createNotHovered
                                    [ fill "red" ]
                                )
                            ]
                        )
                        newEntities
                    )

                _ ->
                    tuple

        False ->
            -- Dict.remove "multiDrag" entities
            tuple