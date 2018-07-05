module LinkSystem exposing (linkSystem)

import Dict exposing (Dict)
import Entity exposing (Entities, Entity, NewEntities)
import Link exposing (..)
import Msgs exposing (Msg)
import LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Shape exposing (Shape, createLineSegment, getCenterPosition, isVectorOver, translateBy)


findParentShape : String -> Entities -> Maybe Shape
findParentShape key entities =
    case Dict.get key entities of
        Just entity ->
            case .shape entity of
                Just nodeShape ->
                    Just nodeShape

                _ ->
                    Nothing

        _ ->
            Nothing


linkSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
linkSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        case .linkComponent entity of
            Just link ->
                let
                    source =
                        getSource (link)

                    target =
                        getTarget (link)
                in
                    case ( findParentShape source entities, findParentShape target entities ) of
                        ( Just sourceShape, Just targetShape ) ->
                            case .shape entity of
                                Just shape ->
                                    ( { entity | shape = Just (createLineSegment (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape ))) }, newEntities )

                                Nothing ->
                                    ( { entity | shape = Just (createLineSegment (LineSegment2d.fromEndpoints ( getCenterPosition sourceShape, getCenterPosition targetShape ))) }, newEntities )

                        _ ->
                            tuple

            _ ->
                tuple
