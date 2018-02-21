module Draggable exposing (Draggable, createDragged, createNotDragged, isDragged, toggleDraggable)


type Draggable
    = Dragged
    | NotDragged


createDragged : Draggable
createDragged =
    Dragged


createNotDragged : Draggable
createNotDragged =
    NotDragged


isDragged : Draggable -> Bool
isDragged drag =
    case drag of
        Dragged ->
            True

        NotDragged ->
            False


toggleDraggable : Draggable -> Draggable
toggleDraggable drag =
    case drag of
        Dragged ->
            NotDragged

        NotDragged ->
            Dragged
