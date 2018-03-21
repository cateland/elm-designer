module UpdateEntitiesBench exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entity
    exposing
        ( Entities
        , createEmptyNewEntities
        )
import Main exposing (updateEntities)
import Model exposing (entities)
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
        [ Benchmark.compare "full model update"
            "old"
            (\_ -> updateEntities msg model)
            "new"
            (\_ -> updateEntities msg model)
        ]


main : BenchmarkProgram
main =
    program suite
