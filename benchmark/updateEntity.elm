module UpdateEntityBench exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entities exposing (box1, box2, circle1, circle2, circle3, circleComponent, entities, link1)
import Entity
    exposing
        ( Entities
        , createEmptyNewEntities
        )
import Main exposing (newUpdateEntity, updateEntity)
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
    describe "Entity update"
        [ Benchmark.compare "box1"
            "old"
            (\_ -> updateEntity msg "box1" box1 ( seed, model.entities ))
            "new"
            (\_ -> newUpdateEntity msg "box1" box1 ( seed, model.entities ))
        , Benchmark.compare "box2"
            "old"
            (\_ -> updateEntity msg "box2" box2 ( seed, model.entities ))
            "new"
            (\_ -> newUpdateEntity msg "box2" box2 ( seed, model.entities ))
        , Benchmark.compare "circleComponent"
            "old"
            (\_ -> updateEntity msg "circleComponent" circleComponent ( seed, model.entities ))
            "new"
            (\_ -> newUpdateEntity msg "circleComponent" circleComponent ( seed, model.entities ))
        , Benchmark.compare "circle1"
            "old"
            (\_ -> updateEntity msg "circle1" circle1 ( seed, model.entities ))
            "new"
            (\_ -> newUpdateEntity msg "circle1" circle1 ( seed, model.entities ))
        , Benchmark.compare "circle2"
            "old"
            (\_ -> updateEntity msg "circle2" circle2 ( seed, model.entities ))
            "new"
            (\_ -> newUpdateEntity msg "circle2" circle2 ( seed, model.entities ))
        , Benchmark.compare "circle3"
            "old"
            (\_ -> updateEntity msg "circle3" circle3 ( seed, model.entities ))
            "new"
            (\_ -> newUpdateEntity msg "circle3" circle3 ( seed, model.entities ))
        , Benchmark.compare "link1"
            "old"
            (\_ -> updateEntity msg "link1" link1 ( seed, model.entities ))
            "new"
            (\_ -> newUpdateEntity msg "link1" link1 ( seed, model.entities ))
        ]


main : BenchmarkProgram
main =
    program suite
