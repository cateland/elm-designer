module DragSystem exposing (..)

import Msgs exposing (Msg(Move))
import Components exposing (Entity, Component(DragStatus), Drag, addComponent)
import DragStatus exposing (getDragStatus, updateDragStatus)


applyDrag : Msgs.Msg -> Entity -> Entity
applyDrag msg entity =
    case getDragStatus entity of
        Just (Components.DragStatus dragStatus) ->
            case dragStatus of
                Just drag ->
                    case msg of
                        Msgs.Move position ->
                            let
                                previousPos = drag.currentPos
                                newDrag =
                                    { drag | previousPos = previousPos, currentPos = position }
                            in
                                updateDragStatus (Components.DragStatus (Just newDrag)) entity

                        Msgs.Release position ->
                            updateDragStatus (Components.DragStatus Nothing) entity

                        _ ->
                            entity

                Nothing ->
                    case msg of
                        Msgs.Press position ->
                            let
                                drag =
                                    { startPos = position
                                    , previousPos = position
                                    , currentPos = position
                                    }
                            in
                                updateDragStatus (Components.DragStatus (Just drag)) entity

                        _ ->
                            entity

        _ ->
            entity