module DraggableSystem exposing (draggableSystem)

import Mouse exposing (Position)
import Draggable exposing (Draggable, createDragged, createNotDragged, isDragged, getDraggStatus, updateCurrentPosition, getDragMoveVectorDelta)
import Entity exposing (Entities, Entity, NewEntities)
import Math exposing (positionToPoint2d)
import Msgs exposing (Msg)
import Shape exposing (isVectorOver, translateBy)


onMoveUpdateEntityDragStatus :
    Draggable.DragStatus
    -> Mouse.Position
    -> Entity
    -> Entity
onMoveUpdateEntityDragStatus dragStatus position entity =
    { entity | drag = Just (createDragged (updateCurrentPosition dragStatus position)) }

onMoveUpdateEntityShape
    : Draggable.DragStatus
    -> Shape.Shape
    -> Entity
    -> Entity
onMoveUpdateEntityShape dragStatus shape entity =
    { entity | shape = Just (translateBy (getDragMoveVectorDelta dragStatus) shape) }


draggableSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
draggableSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        case ( entity.drag, entity.shape ) of
            ( Just dragg, Just shape ) ->
                case isDragged dragg of
                    True ->
                        case msg of
                            Msgs.Move position ->
                                case getDraggStatus dragg of
                                    Just dragStatus ->
                                        let
                                            updatedEntity =
                                                entity
                                                    |> onMoveUpdateEntityDragStatus dragStatus position
                                                    |> onMoveUpdateEntityShape dragStatus shape
                                        in
                                            ( updatedEntity, newEntities )

                                    Nothing ->
                                        tuple

                            Msgs.Release position ->
                                ( { entity | drag = Just createNotDragged }, newEntities )

                            _ ->
                                tuple

                    False ->
                        case msg of
                            Msgs.Press position ->
                                if isVectorOver (positionToPoint2d position) shape then
                                    let
                                        dragStatus =
                                            { startPos = position
                                            , previousPos = position
                                            , currentPos = position
                                            }
                                    in
                                        ( { entity | drag = Just (createDragged dragStatus) }, newEntities )
                                else
                                    tuple

                            _ ->
                                tuple

            _ ->
                tuple
