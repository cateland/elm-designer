module Render exposing (..)

import Components exposing (Component(..))
import Svg exposing (Attribute)
import Svg.Attributes as Attributes exposing (fill, rx, ry, stroke, strokeWidth)


transformAppearenceToAttributes : Components.Attribute -> Attribute msg
transformAppearenceToAttributes appearence =
    case appearence of
        Components.Stroke string ->
            stroke string

        Components.StrokeWidth string ->
            strokeWidth string

        Components.Fill string ->
            fill string

        Components.Rx string ->
            rx string

        Components.Ry string ->
            ry string


generateEntitySvgAttributes : Maybe Component -> List (Svg.Attribute msg)
generateEntitySvgAttributes appearence =
    case appearence of
        Just (Appearance ( initialAppearence, overideAppearence )) ->
            List.append
                (List.map transformAppearenceToAttributes initialAppearence)
                (List.map transformAppearenceToAttributes overideAppearence)

        Nothing ->
            []

        _ ->
            []
