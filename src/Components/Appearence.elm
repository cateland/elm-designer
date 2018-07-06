module Appearence exposing (Appearence, createAppearence, getInitialAppearence, getOverrindingAppearence, setOverrideAppearence, clearOverrideAppearence)

import Attribute exposing (Attribute)


type alias Appearence =
    ( List Attribute, List Attribute )


createAppearence : List Attribute -> List Attribute -> Appearence
createAppearence intialAppearence overridenAppearence =
    ( intialAppearence, intialAppearence )


getInitialAppearence : Appearence -> List Attribute
getInitialAppearence ( intialAppearence, _ ) =
    intialAppearence


getOverrindingAppearence : Appearence -> List Attribute
getOverrindingAppearence ( _, overridenAppearence ) =
    overridenAppearence


setOverrideAppearence : List Attribute -> Appearence -> Appearence
setOverrideAppearence overridenAppearence ( intialAppearence, _ ) =
    ( intialAppearence, overridenAppearence )

clearOverrideAppearence : Appearence -> Appearence
clearOverrideAppearence (intialAppearence, _) =
    (intialAppearence, [])
