module DraggableSystem exposing (..)

import Debug
import Msgs
import Components exposing (Entity, Component)
import Draggable exposing (getDraggable, updateDraggable)
import Shape exposing (..)
import Math exposing (isVectorOver, postionToPoint2d, translateBy)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Math exposing (Drag)


applyDraggable : Msgs.Msg -> Maybe Drag -> Entity -> Entity
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
                ( getDraggable entity, getShape entity, dragging )
            of
                ( Just (Components.Draggable (Components.Dragged)), Just (Components.Shape entityShape), Just drag ) ->
                    updateShape (Components.Shape (translateBy (Vector2d.fromComponents ( toFloat (drag.currentPos.x - drag.previousPos.x), toFloat (drag.currentPos.y - drag.previousPos.y) )) entityShape)) entity

                -- updatePosition (Components.Position position) entity
                _ ->
                    entity

        Msgs.Release position ->
            case
                Debug.log "Release" (getDraggable entity)
            of
                Just (Components.Draggable _) ->
                    updateDraggable (Components.Draggable Components.NotDragged) entity

                _ ->
                    entity
