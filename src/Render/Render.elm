module Render exposing (..)

import Attribute as Attribute exposing (Attribute, transformAppearenceToAttributes)
import Appearence exposing (Appearence, getInitialAppearence, getOverrindingAppearence)
import Svg exposing (Attribute)


generateEntitySvgAttributes : Appearence -> List (Svg.Attribute msg)
generateEntitySvgAttributes appearence =
    List.append
        (List.map transformAppearenceToAttributes (getInitialAppearence appearence))
        (List.map transformAppearenceToAttributes (getOverrindingAppearence appearence))
