module PortSystem exposing (..)

import Dict exposing (Dict)
import Components exposing (Entity, Component(Port, Node, Shape), Port(..), Shape(..))
import Shape exposing (..)
import Port exposing (..)
import Node exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Math exposing (Drag, isVectorOver, postionToPoint2d, translateBy)


findNodeShape : String -> Dict String Entity -> Maybe Shape
findNodeShape key entities =
    case Dict.get key entities of
        Just entity ->
            case ( getShape entity, getNode entity ) of
                ( Just (Shape nodeShape), Just Node ) ->
                    Just nodeShape

                _ ->
                    Nothing

        _ ->
            Nothing


applyPort : Dict String Entity -> Entity -> Entity
applyPort entities entity =
    case ( getPort entity, getShape entity ) of
        ( Just (Port portInfo), Just (Shape portShape) ) ->
            case portInfo of
                PortSource nodeId ->
                    case findNodeShape nodeId entities of
                        Just nodeShape ->
                            case ( nodeShape, portShape ) of
                                ( BoundingBox2d nodeBox, Circle2d portCircle ) ->
                                    let
                                        portPosition =
                                            Circle2d.centerPoint portCircle

                                        nodeBoxTarget =
                                            Point2d.fromCoordinates ( BoundingBox2d.maxX nodeBox, BoundingBox2d.midY nodeBox )

                                        vector =
                                            Vector2d.from portPosition nodeBoxTarget
                                    in
                                        updateShape
                                            (Shape
                                                (translateBy
                                                    vector
                                                    portShape
                                                )
                                            )
                                            entity

                                _ ->
                                    entity

                        Nothing ->
                            entity

                PortSink nodeId ->
                    case findNodeShape nodeId entities of
                        Just nodeShape ->
                            case ( nodeShape, portShape ) of
                                ( BoundingBox2d nodeBox, Circle2d portCircle ) ->
                                    let
                                        portPosition =
                                            Circle2d.centerPoint portCircle

                                        nodeBoxTarget =
                                            Point2d.fromCoordinates ( BoundingBox2d.minX nodeBox, BoundingBox2d.midY nodeBox )

                                        vector =
                                            Vector2d.from portPosition nodeBoxTarget
                                    in
                                        updateShape
                                            (Shape
                                                (translateBy
                                                    vector
                                                    portShape
                                                )
                                            )
                                            entity

                                _ ->
                                    entity

                        Nothing ->
                            entity

        _ ->
            entity
