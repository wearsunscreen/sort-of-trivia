module QuestionTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (all, any, foldl, length, map, member, range)
import List.Extra exposing (unique, zip)
import Question exposing (..)
import Random exposing (Generator, initialSeed, step)
import Test exposing (..)
import Tuple exposing (first, second)


{-| stole this from Random.elm, don't know why I couldn't import it
-}
bool : Generator Bool
bool =
    Random.map ((==) 1) (Random.int 0 1)


{-| Get the set of categories from all the choices of a question
-}
choiceCategories : List Choice -> List Category
choiceCategories q =
    let
        unique list =
            case list of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: xs ->
                    if List.member x xs then
                        unique xs

                    else
                        x :: unique xs
    in
    map (\choice -> choice.category) q


{-| Pick a random set of Categories
-}
randomCategories : Int -> List Category
randomCategories seed =
    let
        cats =
            randomSubset seed allCategories
    in
    case cats of
        [] ->
            [ USCapitals ]

        _ ->
            cats


{-| Pick a random subset of a list
-}
randomSubset : Int -> List a -> List a
randomSubset seed list =
    let
        bools =
            List.range seed (length list + seed)
                |> map (\s -> initialSeed s)
                |> map (\s -> first (step bool s))
                |> zip list

        f : ( a, Bool ) -> List a -> List a
        f ( cat, yesNo ) acc =
            if yesNo then
                cat :: acc

            else
                acc
    in
    foldl f [] bools


{-| This is the suite of tests run by elm-test
-}
suite : Test
suite =
    describe "Test Question.createQuestion"
        [ fuzz int "Seeds should not repeat" <|
            \r ->
                let
                    seed =
                        Random.initialSeed r
                in
                Expect.notEqual seed <| first <| createQuestion seed [ USCapitals ]
        , fuzz int "createQuestion returns a non-empty question" <|
            \r ->
                let
                    ( seed, q ) =
                        createQuestion
                            (Random.initialSeed r)
                            (randomCategories r)
                in
                Expect.true
                    "none of the choices have EmptyCategory"
                    (List.all ((==) True) (List.map (\c -> c.category /= EmptyCategory) q))
        , fuzz int "createQuestion returns 5 unique choices " <|
            \r ->
                let
                    ( seed, q ) =
                        createQuestion
                            (Random.initialSeed r)
                            (randomCategories r)
                in
                Expect.equal 5 <| length <| getChoiceNames q
        , fuzz int "createQuestion sets the directions correctly " <|
            \r ->
                let
                    ( seed, q ) =
                        createQuestion
                            (Random.initialSeed r)
                            (randomCategories r)
                in
                Expect.true "All the directions are correct" (testOptions q)
        , fuzz int "createQuestion returns 5 choices in the given categories" <|
            \r ->
                let
                    cats =
                        randomCategories 23

                    ( seed, q ) =
                        createQuestion (Random.initialSeed 23) cats
                in
                Expect.true "all choices are in the categories specified"
                    (all
                        ((==) True)
                        (map
                            (\c -> member c cats)
                            (choiceCategories q)
                        )
                    )
        , fuzz3 int int int "createQuestion returns choices from multiple categories" <|
            \r1 r2 r3 ->
                let
                    ( _, q1 ) =
                        createQuestion (Random.initialSeed r1) allCategories

                    ( _, q2 ) =
                        createQuestion (Random.initialSeed r2) allCategories

                    ( _, q3 ) =
                        createQuestion (Random.initialSeed r3) allCategories
                in
                Expect.greaterThan 1
                    (length (List.concatMap choiceCategories [ q1, q2, q3 ]))
        , fuzz int "createQuestion returns different questions for different seeds" <|
            \rand1 ->
                let
                    ( _, q1 ) =
                        createQuestion (Random.initialSeed rand1) [ favoredCategory ]

                    ( _, q2 ) =
                        createQuestion (Random.initialSeed (rand1 + 1)) [ favoredCategory ]
                in
                Expect.notEqual q1 q2
        ]


{-| If an choice in in the wrong pot return it to stack
-}
testOptions : List Choice -> Bool
testOptions options =
    let
        cc =
            getCorrectAt CC options

        nw =
            getCorrectAt NW options

        ne =
            getCorrectAt NE options

        se =
            getCorrectAt SE options

        sw =
            getCorrectAt SW options

        testNW : Bool
        testNW =
            List.all ((==) True)
                [ nw.measureX < ne.measureX
                , nw.measureX < cc.measureX
                , nw.measureX < se.measureX
                , nw.measureY > sw.measureY
                , nw.measureY > cc.measureY
                , nw.measureY > se.measureY
                ]

        testNE : Bool
        testNE =
            List.all ((==) True)
                [ ne.measureX > nw.measureX
                , ne.measureX > cc.measureX
                , ne.measureX > sw.measureX
                , ne.measureY > sw.measureY
                , ne.measureY > cc.measureY
                , ne.measureY > se.measureY
                ]

        testSE : Bool
        testSE =
            List.all ((==) True)
                [ se.measureX > nw.measureX
                , se.measureX > cc.measureX
                , se.measureX > sw.measureX
                , se.measureY < cc.measureY
                , se.measureY < ne.measureY
                , se.measureY < nw.measureY
                ]

        testSW : Bool
        testSW =
            List.all ((==) True)
                [ sw.measureX < ne.measureX
                , sw.measureX < cc.measureX
                , sw.measureX < se.measureX
                , sw.measureY < nw.measureY
                , sw.measureY < ne.measureY
                , sw.measureY < cc.measureY
                ]

        testCC : Bool
        testCC =
            List.all ((==) True)
                [ cc.measureX < ne.measureX
                , cc.measureX < se.measureX
                , cc.measureX > nw.measureX
                , cc.measureX > sw.measureX
                , cc.measureY < nw.measureY
                , cc.measureY < ne.measureY
                , cc.measureY > sw.measureY
                , cc.measureY > se.measureY
                ]
    in
    List.all ((==) True)
        [ testNW, testNE, testSE, testSW, testCC ]
