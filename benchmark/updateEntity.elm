module UpdateEntityBench exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entity
    exposing
        ( Entities
        , createEmptyNewEntities
        )
import Main exposing (updateEntity)
import Model exposing (box1, circle1, entities)
import Msgs exposing (Msg(Move))
import Random.Pcg exposing (Seed, initialSeed, step)


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
    in
    describe "AttachementSystem with two elements attached to two others"
        [ Benchmark.compare "entity update"
            "old"
            (\_ -> updateEntity msg "circle1" circle1 ( seed, model.entities ))
            "new"
            (\_ -> updateEntity msg "circle1" circle1 ( seed, model.entities ))
        , Benchmark.compare "entity update"
            "old"
            (\_ -> updateEntity msg "box1" box1 ( seed, model.entities ))
            "new"
            (\_ -> updateEntity msg "box1" box1 ( seed, model.entities ))
        ]


main : BenchmarkProgram
main =
    program suite
