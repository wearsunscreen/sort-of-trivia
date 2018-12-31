module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (length)
import Question exposing (Question, getQuestion)
import Random exposing (initialSeed)
import Test exposing (..)
import Tuple exposing (first, second)


suite : Test
suite =
    describe "Test Sort-of-Trivia"
        [ fuzz int "Seeds should not repeat" <|
            \randomSeed ->
                let
                    seed =
                        Random.initialSeed randomSeed
                in
                Expect.notEqual seed <| first <| getQuestion seed
        , fuzz int "The list of choices should always be 5" <|
            \randSeed ->
                getQuestion (Random.initialSeed randSeed)
                    |> second
                    |> .choices
                    |> length
                    |> Expect.equal 5
        ]
