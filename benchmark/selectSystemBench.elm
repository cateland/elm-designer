module SelectSystemBench exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entities exposing (box1, box2, entities)
import Entity exposing (createEmptyNewEntities)
import Main exposing (Model)
import Msgs exposing (Msg(Move))
import Random.Pcg exposing (Seed, initialSeed, step)
import SelectableSystem exposing (newSelectableSystem, selectableSystem)


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
        [ Benchmark.compare "component with selectable component"
            "old"
            (\_ -> selectableSystem msg entities "box2" ( box2, newEntities ))
            "new"
            (\_ -> newSelectableSystem msg entities "box2" ( box2, newEntities ))
        , Benchmark.compare "component without selectable component"
            "old"
            (\_ -> selectableSystem msg entities "box1" ( box1, newEntities ))
            "new"
            (\_ -> newSelectableSystem msg entities "box1" ( box1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
