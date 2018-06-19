module DragSystemBench exposing (..)

import MultiSelectDragSystem exposing (multiSelectDragSystem, newMultiSelectDragSystem)
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
    describe "DragSystem "
        [ Benchmark.compare "component with attachment"
            "old"
            (\_ -> multiSelectDragSystem msg entities "circle1" ( circle1, newEntities ))
            "new"
            (\_ -> newMultiSelectDragSystem msg entities "circle1" ( circle1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite