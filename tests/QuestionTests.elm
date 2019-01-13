module QuestionTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (all, any, length)
import List.Extra exposing (unique)
import Question exposing (Category(..), Question, createQuestion)
import Random exposing (initialSeed)
import Set exposing (Set, fromList, toList)
import Test exposing (..)
import Tuple exposing (first, second)


isEmptyQuestion : Question -> Bool
isEmptyQuestion q =
    any ((==) True)
        [ q.centerChoice.category == EmptyCategory
        , q.neChoice.category == EmptyCategory
        , q.nwChoice.category == EmptyCategory
        , q.seChoice.category == EmptyCategory
        , q.swChoice.category == EmptyCategory
        ]


choiceNames : Question -> Set String
choiceNames q =
    fromList
        [ q.centerChoice.city
        , q.neChoice.city
        , q.nwChoice.city
        , q.seChoice.city
        , q.swChoice.city
        ]


suite : Test
suite =
    describe "Test Sort-of-Trivia"
        [ fuzz int "Seeds should not repeat" <|
            \randomSeed ->
                let
                    seed =
                        Random.initialSeed randomSeed
                in
                Expect.notEqual seed <| first <| createQuestion seed [ USCapitals ]
        , fuzz int "createQuestion returns a non-empty question" <|
            \randSeed ->
                let
                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) [ USCapitals ]
                in
                Expect.false "is empty" <| isEmptyQuestion q
        , fuzz int "createQuestion returns 5 unique choices " <|
            \randSeed ->
                let
                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) [ USCapitals ]
                in
                Expect.equal 5 <| length <| toList <| choiceNames q
        , fuzz int "createQuestion returns 5 choices in the same category " <|
            \randSeed ->
                let
                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) [ USCapitals ]
                in
                Expect.equal 5 <| length <| toList <| choiceNames q
        , todo "Need to test at it only returns correct categories"
        ]
