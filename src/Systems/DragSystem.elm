module DragSystem exposing (dragSystem)

import Components exposing (Component(DragStatus), Drag)
import DragStatus exposing (getDragStatus, updateDragStatus)
import Entity exposing (Entities, Entity, NewEntities)
import Msgs exposing (Msg)


dragSystem : Msgs.Msg -> Entities -> String -> (Entity, NewEntities) -> (Entity, NewEntities)
dragSystem msg entities key tuple =
    let
        (entity, newEntities) = tuple
    in
        case getDragStatus entity of
            Just (Components.DragStatus dragStatus) ->
                case dragStatus of
                    Just drag ->
                        case msg of
                            Msgs.Move position ->
                                let
                                    previousPos =
                                        drag.currentPos

                                    newDrag =
                                        { drag | previousPos = previousPos, currentPos = position }
                                in
                                (updateDragStatus (Components.DragStatus (Just newDrag)) entity, newEntities)

                            Msgs.Release position ->
                                (updateDragStatus (Components.DragStatus Nothing) entity, newEntities)

                            _ ->
                                tuple

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
                                (updateDragStatus (Components.DragStatus (Just drag)) entity, newEntities)

                            _ ->
                                tuple

            _ ->
                tuple
