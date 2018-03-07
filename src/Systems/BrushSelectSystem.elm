module BrushSelectSystem exposing (brushSelectSystem)

import Brush exposing (getBrush)
import Components exposing (Component(Brush, SelectableComponent, Shape))
import Dict
import Entity exposing (Entities, Entity, NewEntities)
import Msgs exposing (Msg)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Selectable exposing (getSelectable, updateSelectable)
import ShapeComponent exposing (getShape)
import Shape exposing (Shape, getShapeBoundingBox)


findBrushShape : Entities -> Maybe Shape
findBrushShape entities =
    case Dict.get "brush" entities of
        Just entity ->
            case ( getBrush entity, getShape entity ) of
                ( Just (Brush _), Just (Shape brushShape) ) ->
                    Just brushShape

                _ ->
                    Nothing

        _ ->
            Nothing


brushSelectSystem : Msgs.Msg -> Entities -> String -> (Entity, NewEntities) -> (Entity, NewEntities)
brushSelectSystem msg entities key (entity, newEntities) =
    case ( getSelectable entity, getShape entity ) of
        ( Just (SelectableComponent (Components.NotSelected selectedAppearence)), Just (Shape entityShape) ) ->
            case findBrushShape entities of
                Just brushShape ->
                    case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                        True ->
                            (updateSelectable (SelectableComponent (Components.Selected selectedAppearence)) entity, newEntities)

                        False ->
                            (entity, newEntities)

                Nothing ->
                    (entity, newEntities)

        ( Just (SelectableComponent (Components.Selected selectedAppearence)), Just (Shape entityShape) ) ->
            case findBrushShape entities of
                Just brushShape ->
                    case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                        True ->
                            (entity, newEntities)

                        False ->
                            (updateSelectable (SelectableComponent (Components.NotSelected selectedAppearence)) entity, newEntities)

                Nothing ->
                    (entity, newEntities)

        _ ->
            (entity, newEntities)
