module Entity exposing (..)

import Components exposing (Component)
import Dict exposing (Dict)


type Entity
    = Entity (List Component)


type alias Entities =
    Dict String Entity


isComponentEquals : Component -> Component -> Bool
isComponentEquals component1 component2 =
    component1 == component2


addComponent : Component -> Entity -> Entity
addComponent component (Entity components) =
    Entity (component :: components)


removeComponent : Component -> Entity -> Entity
removeComponent component (Entity components) =
    Entity (List.filter (not << (isComponentEquals component)) components)


addEntity : String -> Entity -> Entities -> Entities
addEntity k v entities =
    Dict.insert k v entities
