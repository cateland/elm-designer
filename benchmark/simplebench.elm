module UpdateEntityBench exposing (..)

import Attribute exposing (fill, rx, ry, stroke, strokeWidth)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Components
    exposing
        ( Component(..)
        , Port(PortSink, PortSource)
        )
import Dict exposing (Dict)
import Draggable exposing (createNotDragged)
import Drawable exposing (..)
import Entities exposing (box2, entities)
import Entity
    exposing
        ( Entities
        , Entity
        , NewEntities
        , addEntity
        , createEmptyNewEntities
        , createEntity
        , getNewEntitiesValues
        , getSeed
        , isNewEntitiesEmpty
        )
import Hoverable exposing (createNotHovered)
import Main exposing (newUpdateEntity, updateEntity)
import Msgs exposing (Msg(Move))
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Random.Pcg exposing (Seed, initialSeed, step)
import Shape exposing (Shape(..), createBoundingBox)


type alias Model =
    { entities : Entities
    , currentSeed : Seed
    }


suite : Benchmark
suite =
    let
        msg =
            Move { x = 10, y = 100 }

        seed =
            initialSeed 0

        newEntities =
            createEmptyNewEntities seed

        model =
            Model entities seed

        box3 =
            createEntity
                [ Drawable 70
                , Shape
                    (createBoundingBox
                        (BoundingBox2d.with
                            { minX = 200
                            , maxX = 400
                            , minY = 150
                            , maxY = 250
                            }
                        )
                    )
                , DraggableComponent createNotDragged
                , Appearance
                    ( [ stroke "#C5C5C5"
                      , strokeWidth "2"
                      , fill "#F6F6F6"
                      , rx "4"
                      , ry "4"
                      ]
                    , []
                    )
                , SelectableComponent
                    (Components.NotSelected
                        [ stroke "#67BBFF" ]
                    )
                , HoverableComponent
                    (createNotHovered
                        [ fill "white" ]
                    )
                , Node
                ]
    in
    describe "Entity update"
        [ Benchmark.compare "non existent element"
            "old"
            (\_ -> Dict.insert "box2" box2 entities)
            "new"
            (\_ -> Dict.insert "box2" box3 entities)
        , Benchmark.compare "non existent element"
            "old"
            (\_ -> Dict.get "box2" entities)
            "new"
            (\_ -> Dict.get "box2" entities)
        ]


main : BenchmarkProgram
main =
    program suite
