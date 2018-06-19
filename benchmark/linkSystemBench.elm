module DragSystemBench exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entities exposing (link1, circle1, entities)
import Entity exposing (createEmptyNewEntities)
import LinkSystem exposing (linkSystem, newLinkSystem)
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
        [ Benchmark.compare "component with link"
            "old"
            (\_ -> linkSystem msg entities "link1" ( link1, newEntities ))
            "new"
            (\_ -> newLinkSystem msg entities "link1" ( link1, newEntities ))
        , Benchmark.compare "component without link"
            "old"
            (\_ -> linkSystem msg entities "circle1" ( circle1, newEntities ))
            "new"
            (\_ -> newLinkSystem msg entities "circle1" ( circle1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
