module DraggableSystem exposing (..)

import Msgs exposing (Msg(Press, Move, Release))
import Components exposing (Entity, Component(Shape, Draggable), Draggable(Dragged, NotDragged))
import Draggable exposing (getDraggable, updateDraggable)
import Shape exposing (..)
import Math exposing (Drag, isVectorOver, postionToPoint2d, translateBy)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


applyDraggable : Msgs.Msg -> Maybe Drag -> Entity -> Entity
applyDraggable msg dragging entity =
    case
        msg
    of
        Press position ->
            case
                ( getDraggable entity, getShape entity )
            of
                ( Just (Draggable _), Just (Shape entityShape) ) ->
                    case
                        isVectorOver (postionToPoint2d position) entityShape
                    of
                        True ->
                            updateDraggable (Draggable Dragged) entity

                        False ->
                            entity

                _ ->
                    entity

        Move position ->
            case
                ( getDraggable entity, getShape entity, dragging )
            of
                ( Just (Draggable Dragged), Just (Shape entityShape), Just drag ) ->
                    updateShape
                        (Shape
                            (translateBy
                                (Vector2d.fromComponents ( toFloat (drag.currentPos.x - drag.previousPos.x), toFloat (drag.currentPos.y - drag.previousPos.y) ))
                                entityShape
                            )
                        )
                        entity

                -- updatePosition (Components.Position position) entity
                _ ->
                    entity

        Release position ->
            case
                getDraggable entity
            of
                Just (Draggable _) ->
                    updateDraggable (Draggable NotDragged) entity

                _ ->
                    entity
