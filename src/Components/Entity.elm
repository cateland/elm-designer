module Entity
    exposing
        ( Entities
        , Entity
        , NewEntities
        , addComponent
        , addEntity
        , addToNewEntities
        , addToNewEntitiesWithKey
        , createEmptyNewEntities
        , createEntity
        , getComponents
        , getNewEntitiesValues
        , getSeed
        , isNewEntitiesEmpty
        , removeComponent
        )

import Components exposing (Component)
import Dict exposing (Dict)
import Random.Pcg exposing (Seed, initialSeed, step)
import Uuid


type Entity
    = Entity (List Component)


type alias Entities =
    Dict String Entity


type NewEntities
    = NewEntities ( Seed, List ( String, Entity ) )


createEmptyNewEntities : Seed -> NewEntities
createEmptyNewEntities seed =
    NewEntities ( seed, [] )


addToNewEntities : Entity -> NewEntities -> NewEntities
addToNewEntities entity (NewEntities ( seed, list )) =
    let
        ( newUuid, newSeed ) =
            step Uuid.uuidGenerator seed
    in
    NewEntities ( newSeed, ( toString newUuid, entity ) :: list )


addToNewEntitiesWithKey : String -> Entity -> NewEntities -> NewEntities
addToNewEntitiesWithKey key entity (NewEntities ( seed, list )) =
    NewEntities ( seed, ( key, entity ) :: list )


getSeed : NewEntities -> Seed
getSeed (NewEntities ( seed, _ )) =
    seed


isNewEntitiesEmpty : NewEntities -> Bool
isNewEntitiesEmpty (NewEntities ( _, list )) =
    List.isEmpty list


getNewEntitiesValues : NewEntities -> List ( String, Entity )
getNewEntitiesValues (NewEntities ( _, list )) =
    list


createEntity : List Component -> Entity
createEntity components =
    Entity components


isComponentEquals : Component -> Component -> Bool
isComponentEquals component1 component2 =
    component1 == component2


addComponent : Component -> Entity -> Entity
addComponent component (Entity components) =
    Entity (component :: components)


removeComponent : Component -> Entity -> Entity
removeComponent component (Entity components) =
    Entity (List.filter (not << isComponentEquals component) components)


getComponents : Entity -> List Component
getComponents (Entity components) =
    components


addEntity : String -> Entity -> Entities -> Entities
addEntity k v entities =
    Dict.insert k v entities
