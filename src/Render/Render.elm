module Render exposing (..)

import Attribute as Attribute exposing (Attribute, transformAppearenceToAttributes)
import Components exposing (Component(..))
import Svg exposing (Attribute)


generateEntitySvgAttributes : Maybe Component -> List (Svg.Attribute msg)
generateEntitySvgAttributes appearence =
    case appearence of
        Just (Appearance ( initialAppearence, overideAppearence )) ->
            List.append
                (List.map transformAppearenceToAttributes initialAppearence)
                (List.map transformAppearenceToAttributes overideAppearence)

        _ ->
            []
