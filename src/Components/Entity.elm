module Entity
    exposing
        ( Entities
        , Entity
        , Component
        , NewEntities
        , addEntity
        , addToNewEntities
        , addToNewEntitiesWithKey
        , createEmptyNewEntities
        , getNewEntitiesValues
        , getSeed
        , isNewEntitiesEmpty
        , (<>)
        )

import List exposing ((::), map)
import Dict exposing (Dict)
import Random.Pcg exposing (Seed, initialSeed, step)
import Uuid
import Shape exposing (Shape)
import Draggable exposing (Draggable)


type alias Component =
    Entity -> Entity


type alias Entity =
    { shape : Maybe Shape
    , drawable : Maybe Int
    , drag : Maybe Draggable
    }


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


isComponentEquals : Component -> Component -> Bool
isComponentEquals component1 component2 =
    component1 == component2


(<>) : Entity -> List Component -> Entity
(<>) entity components =
    case components of
        [] ->
            entity

        flip :: fs ->
            (<>) (flip entity) fs


addEntity : String -> Entity -> Entities -> Entities
addEntity k v entities =
    Dict.insert k v entities
