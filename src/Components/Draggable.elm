module Draggable exposing (Draggable, DragStatus, createDragged, createNotDragged, isDragged, getDraggStatus, updateCurrentPosition, getDragMoveVectorDelta)

import Mouse exposing (Position)
import Vector2d as Vector2d exposing (Vector2d)
import Math exposing (positionToPoint2d)


type alias DragStatus =
    { startPos : Mouse.Position
    , previousPos : Mouse.Position
    , currentPos : Mouse.Position
    }


type Draggable
    = Dragged DragStatus
    | NotDragged


createDragged : DragStatus -> Draggable
createDragged status =
    Dragged status


createNotDragged : Draggable
createNotDragged =
    NotDragged


isDragged : Draggable -> Bool
isDragged drag =
    case drag of
        Dragged _ ->
            True

        NotDragged ->
            False


getDraggStatus : Draggable -> Maybe DragStatus
getDraggStatus draggable =
    case draggable of
        Dragged dragStatus ->
            Just dragStatus

        NotDragged ->
            Nothing


updateCurrentPosition : DragStatus -> Mouse.Position -> DragStatus
updateCurrentPosition dragStatus position =
    { dragStatus | previousPos = dragStatus.currentPos, currentPos = position }


getDragMoveVectorDelta : DragStatus -> Vector2d
getDragMoveVectorDelta dragStatus =
    Vector2d.from (positionToPoint2d dragStatus.previousPos) (positionToPoint2d dragStatus.currentPos)
