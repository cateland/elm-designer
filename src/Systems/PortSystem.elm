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


applyPort : Dict String Entity -> Entity -> Entity
applyPort entities entity =
    case ( getPort entity, getShape entity ) of
        ( Just (Port portInfo), Just (Shape portShape) ) ->
            case portInfo of
                PortSource nodeId ->
                    case Dict.get nodeId entities of
                        Just nodeEntity ->
                            case ( getShape nodeEntity, getNode nodeEntity ) of
                                ( Just (Shape nodeShape), Just Node ) ->
                                    case nodeShape of
                                        BoundingBox2d nodeBox ->
                                            case portShape of
                                                Circle2d portCircle ->
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

                                                option2 ->
                                                    entity

                                        _ ->
                                            entity

                                _ ->
                                    entity

                        Nothing ->
                            entity

                PortSink nodeId ->
                    entity

        _ ->
            entity
