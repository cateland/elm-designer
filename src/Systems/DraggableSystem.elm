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
import Entity exposing (Entities, Entity,NewEntities)
import Math exposing (isVectorOver, positionToPoint2d, translateBy)
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


draggableSystem : Msgs.Msg -> Entities -> String -> (Entity, NewEntities) -> (Entity, NewEntities)
draggableSystem msg entities key (entity, newEntities) =
    case ( getDraggable entity, getShape entity, findControlDrag entities ) of
        ( Just (DraggableComponent dragStatus), Just (Shape entityShape), Just drag ) ->
            case
                isVectorOver (positionToPoint2d drag.startPos) entityShape
            of
                True ->
                    case isDragged dragStatus of
                        True ->
                            (updateShape
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
                                entity, newEntities)

                        False ->
                            (updateDraggable (DraggableComponent createDragged) entity, newEntities)

                False ->
                    case isDragged dragStatus of
                        True ->
                            (updateShape
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
                                entity, newEntities)

                        False ->
                            (entity, newEntities)

        ( Just (DraggableComponent _), Just (Shape entityShape), Nothing ) ->
            (updateDraggable (DraggableComponent createNotDragged) entity, newEntities)

        _ ->
            (entity, newEntities)
