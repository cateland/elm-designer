module BrushSystem exposing (brushSystem, newBrushSystem)

import Brush exposing (getBrush, updateBrush)
import Components
    exposing
        ( Component(Appearance, Brush, HoverableComponent, Shape)
        , Drag
        )
import Dict exposing (Dict)
import DragStatus exposing (getDragStatus, updateDragStatus)
import Entity
    exposing
        ( Entities
        , Entity
        , NewEntities
        , addComponent
        , removeComponent
        )
import HoverableComponent exposing (getHoverable, updateHoverable)
import Math exposing (positionToPoint2d)
import Msgs exposing (Msg)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import Shape exposing (createBoundingBox, isVectorOver)
import ShapeComponent exposing (getShape, updateShape)


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
                ( Just (Shape shape), Just (HoverableComponent _) ) ->
                    isVectorOver point shape

                _ ->
                    False


intersectWithEntities : Point2d -> Entities -> Bool
intersectWithEntities point entities =
    Dict.foldl (intersectWithEntity point) False entities


brushSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
brushSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
    case getBrush entity of
        Just (Brush isBrushable) ->
            case ( findControlDrag entities, getShape entity ) of
                ( Just drag, _ ) ->
                    case drag.startPos.x == drag.currentPos.x && drag.startPos.y == drag.currentPos.y of
                        True ->
                            tuple

                        False ->
                            case ( intersectWithEntities (positionToPoint2d drag.startPos) entities, isBrushable ) of
                                ( True, True ) ->
                                    ( updateBrush (Brush False) entity, newEntities )

                                ( False, True ) ->
                                    case getShape entity of
                                        Just _ ->
                                            ( updateShape
                                                (Shape
                                                    (createBoundingBox
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
                                            , newEntities
                                            )

                                        Nothing ->
                                            ( addComponent
                                                (Shape
                                                    (createBoundingBox
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
                                            , newEntities
                                            )

                                _ ->
                                    tuple

                ( Nothing, Just shape ) ->
                    ( removeComponent shape entity, newEntities )

                _ ->
                    ( updateBrush (Brush True) entity, newEntities )

        _ ->
            tuple



newBrushSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
newBrushSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
    case getBrush entity of
        Just (Brush isBrushable) ->
            case ( findControlDrag entities, getShape entity ) of
                ( Just drag, _ ) ->
                    case drag.startPos.x == drag.currentPos.x && drag.startPos.y == drag.currentPos.y of
                        True ->
                            tuple

                        False ->
                            case ( intersectWithEntities (positionToPoint2d drag.startPos) entities, isBrushable ) of
                                ( True, True ) ->
                                    ( updateBrush (Brush False) entity, newEntities )

                                ( False, True ) ->
                                    case getShape entity of
                                        Just _ ->
                                            ( updateShape
                                                (Shape
                                                    (createBoundingBox
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
                                            , newEntities
                                            )

                                        Nothing ->
                                            ( addComponent
                                                (Shape
                                                    (createBoundingBox
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
                                            , newEntities
                                            )

                                _ ->
                                    tuple

                ( Nothing, Just shape ) ->
                    ( removeComponent shape entity, newEntities )

                _ ->
                    ( updateBrush (Brush True) entity, newEntities )

        _ ->
            tuple
