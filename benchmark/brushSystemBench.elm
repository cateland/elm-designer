module BrushSystemBench exposing (..)

import BrushSystem exposing (brushSystem, newBrushSystem)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entities exposing (box1, circle1, entities)
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
    describe "BrushSystem "
        [ Benchmark.compare "component with attachment"
            "old"
            (\_ -> brushSystem msg entities "circle1" ( circle1, newEntities ))
            "new"
            (\_ -> newBrushSystem msg entities "circle1" ( circle1, newEntities ))
        , Benchmark.compare "component without attachment"
            "old"
            (\_ -> brushSystem msg entities "box1" ( box1, newEntities ))
            "new"
            (\_ -> newBrushSystem msg entities "box1" ( box1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
