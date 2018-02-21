module BrushSystem exposing (..)

import Brush exposing (getBrush, updateBrush)
import Components
    exposing
        ( Component(Appearance, Brush, Hoverable, Shape)
        , Drag
        , Hoverable
        , Shape(..)
        )
import Dict exposing (Dict)
import DragStatus exposing (getDragStatus, updateDragStatus)
import Entity
    exposing
        ( Entities
        , Entity
        , addComponent
        , removeComponent
        )
import Hoverable exposing (getHoverable, updateHoverable)
import Math exposing (isVectorOver, postionToPoint2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import Shape exposing (..)


findControlDrag : Entities -> Maybe Drag
findControlDrag entities =
    case Dict.get "control" entities of
        Just entity ->
            case getDragStatus entity of
                Just (Components.DragStatus dragStatus) ->
                    case dragStatus of
                        Just drag ->
                            Just drag

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


intersectWithEntity : Point2d -> String -> Entity -> Bool -> Bool
intersectWithEntity point key entity intersect =
    case intersect of
        True ->
            True

        False ->
            case ( getShape entity, getHoverable entity ) of
                ( Just (Shape shape), Just (Hoverable _) ) ->
                    isVectorOver point shape

                _ ->
                    False


intersectWithEntities : Point2d -> Entities -> Bool
intersectWithEntities point entities =
    Dict.foldl (intersectWithEntity point) False entities


applyBrush : Entities -> Entity -> Entity
applyBrush entities entity =
    case ( getBrush entity, findControlDrag entities, getShape entity ) of
        ( Just (Brush isBrushable), Just drag, _ ) ->
            case drag.startPos.x == drag.currentPos.x && drag.startPos.y == drag.currentPos.y of
                True ->
                    entity

                False ->
                    case ( intersectWithEntities (postionToPoint2d drag.startPos) entities, isBrushable ) of
                        ( True, True ) ->
                            updateBrush (Brush False) entity

                        ( False, True ) ->
                            case getShape entity of
                                Just _ ->
                                    updateShape
                                        (Shape
                                            (BoundingBox2d
                                                (BoundingBox2d.with
                                                    { minX = toFloat drag.startPos.x
                                                    , maxX = toFloat drag.currentPos.x
                                                    , minY = toFloat drag.startPos.y
                                                    , maxY = toFloat drag.currentPos.y
                                                    }
                                                )
                                            )
                                        )
                                        entity

                                Nothing ->
                                    addComponent
                                        (Shape
                                            (BoundingBox2d
                                                (BoundingBox2d.with
                                                    { minX = toFloat drag.startPos.x
                                                    , maxX = toFloat drag.currentPos.x
                                                    , minY = toFloat drag.startPos.y
                                                    , maxY = toFloat drag.currentPos.y
                                                    }
                                                )
                                            )
                                        )
                                        entity

                        _ ->
                            entity

        ( Just (Brush _), Nothing, Just shape ) ->
            removeComponent shape entity

        _ ->
            updateBrush (Brush True) entity
