module PortSystemBench exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entities exposing (box1, circle1, circle3, entities)
import Entity exposing (createEmptyNewEntities)
import Main exposing (Model)
import Msgs exposing (Msg(Move))
import Random.Pcg exposing (Seed, initialSeed, step)
import PortSystem exposing (portSystem, newPortSystem)


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
    describe "SelectSystem "
        [ Benchmark.compare "component with port component"
            "old"
            (\_ -> portSystem msg entities "circle1" ( circle1, newEntities ))
            "new"
            (\_ -> newPortSystem msg entities "circle1" ( circle1, newEntities ))
        , Benchmark.compare "component wit port component and attachement"
            "old"
            (\_ -> portSystem msg entities "circle3" ( circle3, newEntities ))
            "new"
            (\_ -> newPortSystem msg entities "circle3" ( circle3, newEntities ))
        , Benchmark.compare "component without port component"
            "old"
            (\_ -> portSystem msg entities "box1" ( box1, newEntities ))
            "new"
            (\_ -> newPortSystem msg entities "box1" ( box1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
