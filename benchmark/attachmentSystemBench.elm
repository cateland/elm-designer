module AttachmentSystemBench exposing (..)

import AttachmentSystem exposing (attachementSystem, newAttachementSystem)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Entity exposing (createEmptyNewEntities)
import Main exposing (Model)
import Entities exposing (box1, circle1, entities)
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
    describe "AttachementSystem with two elements attached to two others"
        [ Benchmark.compare "component with attachment"
            "old"
            (\_ -> attachementSystem msg entities "circle1" ( circle1, newEntities ))
            "new"
            (\_ -> newAttachementSystem msg entities "circle1" ( circle1, newEntities ))
        , Benchmark.compare "component without attachment"
            "old"
            (\_ -> attachementSystem msg entities "box1" ( box1, newEntities ))
            "new"
            (\_ -> newAttachementSystem msg entities "box1" ( box1, newEntities ))
        ]


main : BenchmarkProgram
main =
    program suite
