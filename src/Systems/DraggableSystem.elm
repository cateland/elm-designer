module DraggableSystem exposing (..)

import Msgs
import Components exposing (Entity)
import Draggable exposing (getDraggable, updateDraggable)
import Shape exposing (..)
import Math exposing (isVectorOver, postionToPoint2d)


applyDraggable : Msgs.Msg -> a -> Entity -> Entity
applyDraggable msg dragging entity =
    case
        msg
    of
        Msgs.Press position ->
            case
                ( getDraggable entity, getShape entity )
            of
                ( Just (Components.Draggable _), Just (Components.Shape entityShape) ) ->
                    case
                        isVectorOver (postionToPoint2d position) entityShape
                    of
                        True ->
                            updateDraggable (Components.Draggable Components.Dragged) entity

                        False ->
                            entity

                _ ->
                    entity

        Msgs.Move position ->
            case
                ( getDraggable entity, getShape entity )
            of
                ( Just (Components.Draggable (Components.Dragged)), Just (Components.Shape entityShape) ) ->
                    entity

                -- updatePosition (Components.Position position) entity
                _ ->
                    entity

        Msgs.Release position ->
            case
                (getDraggable entity)
            of
                Just (Components.Draggable _) ->
                    updateDraggable (Components.Draggable Components.NotDragged) entity

                _ ->
                    entity
