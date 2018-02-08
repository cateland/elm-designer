module Render exposing (..)

import Svg exposing (Attribute)
import Svg.Attributes as Attributes exposing (stroke, strokeWidth, fill)
import Components exposing (Component(..))


transformAppearenceToAttributes : Components.Attribute -> Attribute msg
transformAppearenceToAttributes appearence =
    case appearence of
        Components.Stroke string ->
            stroke string

        Components.StrokeWidth string ->
            strokeWidth string

        Components.Fill string ->
            fill string


generateEntitySvgAttributes : Maybe Component -> List (Svg.Attribute msg)
generateEntitySvgAttributes appearence =
    case appearence of
        Just (Appearance appearence) ->
            List.map transformAppearenceToAttributes appearence

        Nothing ->
            []

        _ ->
            []
