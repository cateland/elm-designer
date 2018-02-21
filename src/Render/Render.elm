module Render exposing (..)

import Attribute as Attribute exposing (Attribute)
import Svg exposing (Attribute)
import Components exposing (Component(..))
import Attribute exposing (transformAppearenceToAttributes)



generateEntitySvgAttributes : Maybe Component -> List (Svg.Attribute msg)
generateEntitySvgAttributes appearence =
    case appearence of
        Just (Appearance ( initialAppearence, overideAppearence )) ->
            List.append
                (List.map transformAppearenceToAttributes initialAppearence)
                (List.map transformAppearenceToAttributes overideAppearence)


        _ ->
            []
