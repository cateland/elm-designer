module MathTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer, int)
import Math exposing (positionToPoint2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import Test exposing (Test, describe, fuzz, test)
import Random.Pcg as Random
import Shrink

-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!



type alias Position =
    { x : Int, y : Int }

position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))

wipFuzzSuite : Test
wipFuzzSuite =
    describe "Math module"
        [ describe "postionToPoint2d"
            [ fuzz position "position.x is properly mapped to Point2d x coordinates" <|
                \generatedPositionx ->
                    let

                        generatedPoint2d =
                            positionToPoint2d generatedPositionx
                    in
                    Expect.equal (round (Point2d.xCoordinate generatedPoint2d)) generatedPositionx.x
            , fuzz position "position.y is properly mapped to Point2d y coordinates" <|
                \generatedPosition ->
                    let

                        generatedPoint2d =
                            positionToPoint2d generatedPosition
                    in
                    Expect.equal (round (Point2d.yCoordinate generatedPoint2d)) generatedPosition.y
            ]
        ]
