module BrushSelectSystem exposing (..)

import Brush exposing (getBrush)
import Components exposing (Component(Brush, Selectable, Shape), Shape)
import Dict
import Entity exposing (Entities, Entity)
import Math exposing (getShapeBoundingBox)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Selectable exposing (getSelectable, updateSelectable)
import Shape exposing (getShape)


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


applyBrushSelect : Entities -> Entity -> Entity
applyBrushSelect entities entity =
    case ( getSelectable entity, getShape entity ) of
        ( Just (Selectable (Components.NotSelected selectedAppearence)), Just (Shape entityShape) ) ->
            case findBrushShape entities of
                Just brushShape ->
                    case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                        True ->
                            updateSelectable (Selectable (Components.Selected selectedAppearence)) entity

                        False ->
                            entity

                Nothing ->
                    entity

        ( Just (Selectable (Components.Selected selectedAppearence)), Just (Shape entityShape) ) ->
            case findBrushShape entities of
                Just brushShape ->
                    case BoundingBox2d.isContainedIn (getShapeBoundingBox brushShape) (getShapeBoundingBox entityShape) of
                        True ->
                            entity

                        False ->
                            updateSelectable (Selectable (Components.NotSelected selectedAppearence)) entity

                Nothing ->
                    entity

        _ ->
            entity
