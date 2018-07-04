module BrushSelectSystem exposing (brushSelectSystem, newBrushSelectSystem)

import Brush exposing (getBrush)
import Components exposing (Component(Brush, SelectableComponent, ShapeComponent))
import Dict
import Entity exposing (Entities, Entity, NewEntities)
import Msgs exposing (Msg)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Selectable exposing (getSelectable, updateSelectable)
import Shape exposing (Shape, getShapeBoundingBox)
import ShapeComponent exposing (getShape)


findBrushShape : Entities -> Maybe Shape
findBrushShape entities =
    case Dict.get "brush" entities of
        Just entity ->
            case ( getBrush entity, getShape entity ) of
                ( Just (Brush _), Just (ShapeComponent brushShape) ) ->
                    Just brushShape

                _ ->
                    Nothing

        _ ->
            Nothing


brushSelectSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
brushSelectSystem msg entities key tuple =
    let
        ( entity, newEntities ) = tuple
    in
    case getSelectable entity of
        Just (SelectableComponent (Components.NotSelected selectedAppearence)) ->
            case getShape entity of
                Just (ShapeComponent entityShape) ->
                    case findBrushShape entities of
                        Just brushShape ->
                            case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                                True ->
                                    ( updateSelectable (SelectableComponent (Components.Selected selectedAppearence)) entity, newEntities )

                                False ->
                                    tuple

                        Nothing ->
                            tuple

                _ ->
                    tuple

        Just (SelectableComponent (Components.Selected selectedAppearence)) ->
            case getShape entity of
                Just (ShapeComponent entityShape) ->
                    case findBrushShape entities of
                        Just brushShape ->
                            case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                                True ->
                                    tuple

                                False ->
                                    ( updateSelectable (SelectableComponent (Components.NotSelected selectedAppearence)) entity, newEntities )

                        Nothing ->
                            tuple

                _ ->
                    tuple

        _ ->
            tuple


newBrushSelectSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
newBrushSelectSystem msg entities key tuple =
    let
        ( entity, newEntities ) = tuple
    in
    case getSelectable entity of
        Just (SelectableComponent (Components.NotSelected selectedAppearence)) ->
            case getShape entity of
                Just (ShapeComponent entityShape) ->
                    case findBrushShape entities of
                        Just brushShape ->
                            case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                                True ->
                                    ( updateSelectable (SelectableComponent (Components.Selected selectedAppearence)) entity, newEntities )

                                False ->
                                    tuple

                        Nothing ->
                            tuple

                _ ->
                    tuple

        Just (SelectableComponent (Components.Selected selectedAppearence)) ->
            case getShape entity of
                Just (ShapeComponent entityShape) ->
                    case findBrushShape entities of
                        Just brushShape ->
                            case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                                True ->
                                    tuple

                                False ->
                                    ( updateSelectable (SelectableComponent (Components.NotSelected selectedAppearence)) entity, newEntities )

                        Nothing ->
                            tuple

                _ ->
                    tuple

        _ ->
            tuple
