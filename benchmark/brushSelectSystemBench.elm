module BrushSelectSystemBench exposing (..)

import BrushSelectSystem exposing (brushSelectSystem, newBrushSelectSystem)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
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
    describe "HovereableSystem "
        [ Benchmark.compare "component with selectable component"
            "old"
            (\_ -> brushSelectSystem msg entities "box2" ( box2, newEntities ))
            "new"
            (\_ -> newBrushSelectSystem msg entities "box2" ( box2, newEntities ))
        , Benchmark.compare "component without selectable component"
            "old"
            (\_ -> brushSelectSystem msg entities "circle1" ( circle1, newEntities ))
            "new"
            (\_ -> newBrushSelectSystem msg entities "circle1" ( circle1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
