module Hoverable
    exposing
        ( Hoverable
        , createHovered
        , createNotHovered
        , getHoverAppearence
        , isHovered
        , toggleHoverrable
        )

import Attribute exposing (Attribute)


type Hoverable
    = Hovered (List Attribute)
    | NotHovered (List Attribute)


createHovered : List Attribute -> Hoverable
createHovered appearence =
    Hovered appearence


createNotHovered : List Attribute -> Hoverable
createNotHovered appearence =
    NotHovered appearence


isHovered : Hoverable -> Bool
isHovered hoverable =
    case hoverable of
        Hovered _ ->
            True

        NotHovered _ ->
            False


getHoverAppearence : Hoverable -> List Attribute
getHoverAppearence hoverable =
    case hoverable of
        Hovered appearence ->
            appearence

        NotHovered appearence ->
            appearence


toggleHoverrable : Hoverable -> Hoverable
toggleHoverrable hoverable =
    case hoverable of
        Hovered appearence ->
            NotHovered appearence

        NotHovered appearence ->
            Hovered appearence
