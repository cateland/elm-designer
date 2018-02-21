module Attribute
    exposing
        ( Attribute
        , transformAppearenceToAttributes
        , stroke
        , strokeWidth
        , fill
        , rx
        , ry
        )

import Svg exposing (Attribute)
import Svg.Attributes exposing (stroke, strokeWidth, fill, rx, ry)


type Attribute
    = Stroke String
    | StrokeWidth String
    | Fill String
    | Rx String
    | Ry String


transformAppearenceToAttributes : Attribute -> Svg.Attribute msg
transformAppearenceToAttributes appearence =
    case appearence of
        Stroke string ->
            Svg.Attributes.stroke string

        StrokeWidth string ->
            Svg.Attributes.strokeWidth string

        Fill string ->
            Svg.Attributes.fill string

        Rx string ->
            Svg.Attributes.rx string

        Ry string ->
            Svg.Attributes.ry string


stroke : String -> Attribute
stroke value =
    Stroke value


strokeWidth : String -> Attribute
strokeWidth value =
    StrokeWidth value


fill : String -> Attribute
fill value =
    Fill value


rx : String -> Attribute
rx value =
    Rx value


ry : String -> Attribute
ry value =
    Ry value
