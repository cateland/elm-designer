module DraggableSystem exposing (..)

import Dict exposing (Dict)
import Components
    exposing
        ( Entities
        , Entity
        , Component(Shape, Draggable, DragStatus)
        , Draggable(Dragged, NotDragged)
        , Drag
        )
import Draggable exposing (getDraggable, updateDraggable)
import Shape exposing (..)
import DragStatus exposing (getDragStatus, updateDragStatus)
import Math exposing (isVectorOver, postionToPoint2d, translateBy)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)

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


applyDraggable : Entities -> Entity -> Entity
applyDraggable entities entity =
    case ( getDraggable entity, getShape entity, findControlDrag entities ) of
        ( Just (Draggable dragStatus), Just (Shape entityShape), Just drag ) ->
            case
                isVectorOver (postionToPoint2d drag.startPos) entityShape
            of
                True ->
                    case dragStatus of
                        Dragged ->
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

                        NotDragged ->
                            updateDraggable (Draggable Dragged) entity

                False ->
                    case dragStatus of
                        Dragged ->
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

                        NotDragged ->
                            entity

        ( Just (Draggable _), Just (Shape entityShape), Nothing ) ->
            updateDraggable (Draggable NotDragged) entity

        _ ->
            entity
