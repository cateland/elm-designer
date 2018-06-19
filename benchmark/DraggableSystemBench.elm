module DraggableSystemBench exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import DraggableSystem exposing (draggableSystem, newDraggableSystem)
import Entities exposing (box2, circle1, entities)
import Entity exposing (createEmptyNewEntities)
import Main exposing (Model)
import Msgs exposing (Msg(Move))
import Random.Pcg exposing (Seed, initialSeed, step)


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
    in
    describe "DragSystem "
        [ Benchmark.compare "component with draggable"
            "old"
            (\_ -> draggableSystem msg entities "box2" ( box2, newEntities ))
            "new"
            (\_ -> newDraggableSystem msg entities "box2" ( box2, newEntities ))
        , Benchmark.compare "component without draggable"
            "old"
            (\_ -> draggableSystem msg entities "circle1" ( circle1, newEntities ))
            "new"
            (\_ -> newDraggableSystem msg entities "circle1" ( circle1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
