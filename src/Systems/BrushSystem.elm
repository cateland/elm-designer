module BrushSystem exposing (..)

import Msgs exposing (Msg(Move))
import Components
    exposing
        ( Entities
        , Entity
        , Component(Shape, Hoverable, Appearance, Brush)
        , Drag
        , Shape(..)
        , Hoverable
        , addComponent
        )
import Hoverable exposing (getHoverable, updateHoverable)
import Appearance exposing (getAppearance, updateAppearance)
import Brush exposing (getBrush)
import DragStatus exposing (getDragStatus, updateDragStatus)
import Shape exposing (..)
import Math exposing (isVectorOver, postionToPoint2d)
import Dict exposing (Dict)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)


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


applyBrush : Msgs.Msg -> Entities -> Entity -> Entity
applyBrush msg entities entity =
    case ( getBrush entity, findControlDrag entities ) of
        ( Just Brush, Just drag ) ->
            case drag.startPos.x == drag.currentPos.x && drag.startPos.y == drag.currentPos.y of
                True ->
                    entity

                False ->
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

        ( Just Brush, Nothing ) ->
            entity

        _ ->
            entity
