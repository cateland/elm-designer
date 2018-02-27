module DraggableSystem exposing (draggableSystem)

import Components
    exposing
        ( Component(DragStatus, DraggableComponent, Shape)
        , Drag
        )
import Dict exposing (Dict)
import DragStatus exposing (getDragStatus, updateDragStatus)
import Draggable exposing (createDragged, createNotDragged, isDragged)
import DraggableComponent exposing (getDraggable, updateDraggable)
import Entity exposing (Entities, Entity)
import Math exposing (isVectorOver, postionToPoint2d, translateBy)
import Msgs exposing (Msg)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
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


draggableSystem : Msgs.Msg -> Entities -> String -> Entity -> Entity
draggableSystem msg entities key entity =
    case ( getDraggable entity, getShape entity, findControlDrag entities ) of
        ( Just (DraggableComponent dragStatus), Just (Shape entityShape), Just drag ) ->
            case
                isVectorOver (postionToPoint2d drag.startPos) entityShape
            of
                True ->
                    case isDragged dragStatus of
                        True ->
                            updateShape
                                (Shape
                                    (translateBy
                                        (Vector2d.fromComponents
                                            ( toFloat (drag.currentPos.x - drag.previousPos.x)
                                            , toFloat (drag.currentPos.y - drag.previousPos.y)
                                            )
                                        )
                                        entityShape
                                    )
                                )
                                entity

                        False ->
                            updateDraggable (DraggableComponent createDragged) entity

                False ->
                    case isDragged dragStatus of
                        True ->
                            updateShape
                                (Shape
                                    (translateBy
                                        (Vector2d.fromComponents
                                            ( toFloat (drag.currentPos.x - drag.previousPos.x)
                                            , toFloat (drag.currentPos.y - drag.previousPos.y)
                                            )
                                        )
                                        entityShape
                                    )
                                )
                                entity

                        False ->
                            entity

        ( Just (DraggableComponent _), Just (Shape entityShape), Nothing ) ->
            updateDraggable (DraggableComponent createNotDragged) entity

        _ ->
            entity
