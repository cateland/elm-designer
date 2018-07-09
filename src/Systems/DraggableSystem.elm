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


onMoveUpdateEntityShape :
    Draggable.DragStatus
    -> Shape.Shape
    -> Entity
    -> Entity
onMoveUpdateEntityShape dragStatus shape entity =
    { entity | shape = Just (translateBy (getDragMoveVectorDelta dragStatus) shape) }


onMouseMove : Draggable -> Position -> Shape.Shape -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
onMouseMove dragg position shape tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
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


onMouseRelease : ( Entity, NewEntities ) -> ( Entity, NewEntities )
onMouseRelease tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        ( { entity | drag = Just createNotDragged }, newEntities )


onMousePress : Position -> Shape.Shape -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
onMousePress position shape tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
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


update : ( Entity, NewEntities ) -> Msg -> Draggable -> Shape.Shape -> ( Entity, NewEntities )
update tuple msg dragg shape =
    let
        ( entity, newEntities ) =
            tuple
    in
        case isDragged dragg of
            True ->
                case msg of
                    Msgs.Move position ->
                        onMouseMove dragg position shape tuple

                    Msgs.Release position ->
                        onMouseRelease tuple

                    _ ->
                        tuple

            False ->
                case msg of
                    Msgs.Press position ->
                        onMousePress position shape tuple

                    _ ->
                        tuple


draggableSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
draggableSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        Maybe.map2 (update tuple msg) entity.drag entity.shape
            |> Maybe.withDefault tuple
