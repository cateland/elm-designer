module HoverableSystemBench exposing (..)

import HoverableSystem exposing (hoverableSystem, newHoverableSystem)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entities exposing (box1, circleComponent, entities)
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
        [ Benchmark.compare "component with hoverable component"
            "old"
            (\_ -> hoverableSystem msg entities "circleComponent" ( circleComponent, newEntities ))
            "new"
            (\_ -> newHoverableSystem msg entities "circleComponent" ( circleComponent, newEntities ))
        , Benchmark.compare "component without hoverable component"
            "old"
            (\_ -> hoverableSystem msg entities "box1" ( box1, newEntities ))
            "new"
            (\_ -> newHoverableSystem msg entities "box1" ( box1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
