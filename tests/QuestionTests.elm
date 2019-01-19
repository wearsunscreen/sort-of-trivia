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
choiceCategories : Question -> List Category
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


{-| Get all the names from all the choices of a question
-}
choiceNames : Question -> List String
choiceNames q =
    map (\choice -> choice.name) q


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
                Expect.greaterThan 0 <|
                    String.length <|
                        getChoiceNameAt iCC q
        , fuzz int "createQuestion returns 5 unique choices " <|
            \r ->
                let
                    ( seed, q ) =
                        createQuestion
                            (Random.initialSeed r)
                            (randomCategories r)
                in
                Expect.equal 5 <| length <| choiceNames q
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
