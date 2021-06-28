module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import IntervalCycles exposing (..)
import PcInt exposing (Edo)
import PitchClass
import PcSetBasics exposing (PcSet(..))
import Transformations exposing (..)
import Helpers exposing (..)
import Array
import Html.Attributes exposing (wrap)


edo12 : Edo
edo12 = PcInt.edoFromInt 12

suite : Test
suite =
    describe "Test the PitchClass and PcSetBasics modules"
        [ describe "PitchClass"
            [ test "parse \"Cb\" as a pitch class and convert to an Int" <|
                \_ ->
                    "Cb"
                        |> PitchClass.parsePitchClass
                        |> Maybe.map PitchClass.toInt
                        |> Expect.equal (Just 11)
            , test "parse \"Fx\" as a pitch class and convert to a String" <|
                \_ ->
                    "Fx"
                        |> PitchClass.parsePitchClass
                        |> Maybe.map PitchClass.toString
                        |> Expect.equal (Just "F𝄪")
            ]
        , describe "transpose all rotations to 0"
            [ test "try with 1 4 8" <|
                \_ ->
                    PcInt.listFromInput edo12 "1 4 8"
                        |> Helpers.rotationalArrays
                        |> List.map (PcSetBasics.transposeToZero edo12)
                        |> List.map (PcSet edo12)
                        |> List.map PcSetBasics.setToString
                        |> Expect.equal ["{0, 3, 7}", "{0, 4, 9}", "{0, 5, 8}"]
            ]
        , describe "transpose a pc set"
            [ test "T2 047" <|
                \_ ->
                    PcSet edo12 (PcInt.listFromInput edo12 "0 4 7")
                        |> PcSetBasics.transposeSet 2
                        |> PcSetBasics.setToString
                        |> Expect.equal "{2, 6, 9}"
            , test "T8 3,10,15,20 (edo24)" <|
                \_ ->
                    let edo24 = (PcInt.edoFromInt 24)
                    in
                    PcSet edo24 (PcInt.listFromInput edo24 "3 10 15 20")
                        |> PcSetBasics.transposeSet 8
                        |> PcSetBasics.setToString
                        |> Expect.equal "{11, 18, 23, 4}"
            ]
        , describe "invert a pc set"
            [ test "I3 2580" <|
                \_ ->
                    PcSet edo12 (PcInt.listFromInput edo12 "0 2 5 8")
                        |> PcSetBasics.invertSet 3
                        |> PcSetBasics.setToString
                        |> Expect.equal "{7, 10, 1, 3}"
            ]
        , describe "prime form (assuming Rahn algorithm)"
            [ test "5 11 3 8 (original form is most compact)" <|
                \_ ->
                    PcSet edo12 (PcInt.listFromInput edo12 "5 11 3 8")
                        |> PcSetBasics.primeForm
                        |> PcSetBasics.setToString
                        |> Expect.equal "{0, 2, 5, 8}"
            , test "4 8 11 2 (inverted form is most compact)" <|
                \_ ->
                    PcSet edo12 (PcInt.listFromInput edo12 "4 8 11 2")
                        |> PcSetBasics.primeForm
                        |> PcSetBasics.setToString
                        |> Expect.equal "{0, 2, 5, 8}"
            ]

        , describe "determine the normal form of a set"
            [ test "try with [1,4,8] (already in normal form)" <|
                \_ -> 
                    PcSet edo12 (PcInt.listFromInput edo12 "1 4 8")
                        |> PcSetBasics.normalForm
                        |> PcSetBasics.setToString
                        |> Expect.equal "{1, 4, 8}"
            , test "try with [1, 2, 10] (needs a rotation)" <|
                \_ -> 
                    PcSet edo12 (PcInt.listFromInput edo12 "1 2 10")
                        |> PcSetBasics.normalForm
                        |> PcSetBasics.setToString
                        |> Expect.equal "{10, 1, 2}" 
            ]
        , describe "aggregate function"
            [ test "aggregate of modulus 12" <|
                \_ ->
                    PcSetBasics.aggregate edo12
                        |> PcSetBasics.setToString
                        |> Expect.equal "{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}"
            ]
        , describe "complement function"
            [ test "complement of 024579B" <|
                \_ ->
                    PcSet edo12 (PcInt.listFromInput edo12 "0 2 4 5 7 9 11")
                        |> PcSetBasics.complement
                        |> PcSetBasics.setToString
                        |> Expect.equal "{1, 3, 6, 8, 10}"
            , fuzz (Fuzz.intRange 2 96) "the complement of an empty set is the aggregate" <|
                \x ->
                    let
                        edo = PcInt.edoFromInt x
                    in
                    PcSet edo (PcInt.listFromInput edo "")
                        |> PcSetBasics.complement
                        |> PcSetBasics.setToString
                        |> Expect.equal (PcSetBasics.setToString <| PcSetBasics.aggregate edo)
            ]
        , describe "find transformations"
            [ test "247 is T_2 of 025" <|
                \_ ->
                    let
                        setA = PcSet edo12 (PcInt.listFromInput edo12 "0 2 5")
                        setB = PcSet edo12 (PcInt.listFromInput edo12 "2 4 7")
                    in
                    possibleTransformations setA setB
                        |> List.map transformationToString
                        |> String.join ""
                        |> Expect.equal "T_2"
            , test "047 is I_7 of 037" <|
                \_ ->
                    let
                        setA = PcSet edo12 (PcInt.listFromInput edo12 "0 4 7")
                        setB = PcSet edo12 (PcInt.listFromInput edo12 "0 3 7")
                    in
                    possibleTransformations setA setB
                        |> List.map transformationToString
                        |> String.join ""
                        |> Expect.equal "I_7"

            ]
        ]



helpers : Test
helpers = 
    describe "Helper functions"
        [ describe "testing rotations"
            [ test "rotate arrays" <|
                \_ ->
                    Helpers.rotationalArrays [1,2,3,4] 
                        |> Expect.equal [ [1,2,3,4], [2,3,4,1], [3,4,1,2], [4,1,2,3] ]
            ]
        , describe "helper methods for list sorting"
            [ test "comparing pc ints 9 to 11" <|
                \_ ->
                    let 
                        pcA = PcInt.pcInt edo12 9
                        pcB = PcInt.pcInt edo12 11
                    in
                    PcSetBasics.comparePcInts pcA pcB
                        |> Expect.equal LT
            , test "comparing pc ints 5 to 0" <|
                \_ ->
                    let 
                        pcA = PcInt.pcInt edo12 5
                        pcB = PcInt.pcInt edo12 0
                    in
                    PcSetBasics.comparePcInts pcA pcB
                        |> Expect.equal GT
            , test "comparing pc ints 4 to 4 (edo=31)" <|
                \_ ->
                    let 
                        pcA = PcInt.pcInt (PcInt.edoFromInt 31) 4
                        pcB = PcInt.pcInt (PcInt.edoFromInt 31) 4
                    in
                    PcSetBasics.comparePcInts pcA pcB
                        |> Expect.equal EQ
            , test "comparing lists" <|
                \_ ->
                    compareLists compare [7, 6, 4, 0] [7, 6, 3, 0]
                        |> Expect.equal GT
            ]
        , describe "wrapping lists" <|
            [ test "wrap list [1,2,3,4]" <|
                \_ ->
                    wrapList [1, 2, 3, 4]
                        |> Expect.equal [1, 2, 3, 4, 1]
            , test "wrap list [\"foo\"]" <|
                \_ ->
                    wrapList ["foo"]
                        |> Expect.equal ["foo", "foo"]
            , test "wrap list []" <|
                \_ ->
                    wrapList []
                        |> Expect.equal []
            ]
        , describe "rotate to initial nothing" 
            [ test "{1, 2, _} -> {_, 1, 2}" <|
                \_ ->
                    orderToInitialNothing [Just 1, Just 2, Nothing]
                        |> Expect.equal [Nothing, Just 1, Just 2]
            , test "{1, 2, _, 4} -> {_, 4, 1, 2}" <|
                \_ ->
                    orderToInitialNothing [Just 1, Just 2, Nothing, Just 4]
                        |> Expect.equal [Nothing, Just 4, Just 1, Just 2]
            , test "{_, 1, 2} -> {_, 1, 2}" <|
                \_ ->
                    orderToInitialNothing [Nothing, Just 1, Just 2]
                        |> Expect.equal [Nothing, Just 1, Just 2]
            , test "{1, 2, _, 4, 5, _, 7} -> {_, 4, 5, _, 7, 1, 2}" <|
                \_ ->
                    orderToInitialNothing [Just 1, Just 2, Nothing, Just 4, Just 5, Nothing, Just 7]
                        |> Expect.equal [Nothing, Just 4, Just 5, Nothing, Just 7, Just 1, Just 2]
            ]
        ]


testGenericIntervalCycle : Int -> Int -> List (List (Maybe Int)) -> Expectation
testGenericIntervalCycle modulus interval expectedOutput =
    genericIntervalCycle modulus interval
        |> Expect.equal expectedOutput


intervalCycleTests : Test
intervalCycleTests =
    describe "Testing the Interval Cycle module and whatnot"
        [ describe "tests for generic interval cycles, mod 12" 
            [ test "1-cycle" <|
                \_ -> 
                    [ [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] ]
                    |> List.map (List.map (\i -> Just i) )
                    |> testGenericIntervalCycle 12 1 
            , test "2-cycle" <|
                \_ -> 
                    [ [0, 2, 4, 6, 8, 10], [1, 3, 5, 7, 9, 11] ]
                    |> List.map (List.map (\i -> Just i) )
                    |> testGenericIntervalCycle 12 2 
            , test "3-cycle" <|
                \_ -> 
                    [ [0, 3, 6, 9], [1, 4, 7, 10], [2, 5, 8, 11] ]
                    |> List.map (List.map (\i -> Just i) )
                    |> testGenericIntervalCycle 12 3 
            , test "4-cycle" <|
                \_ -> 
                    [ [0, 4, 8], [1, 5, 9], [2, 6, 10], [3, 7, 11] ]
                    |> List.map (List.map (\i -> Just i) )
                    |> testGenericIntervalCycle 12 4 
            , test "5-cycle" <|
                \_ -> 
                    [ [0, 5, 10, 3, 8, 1, 6, 11, 4, 9, 2, 7] ]
                    |> List.map (List.map (\i -> Just i) )
                    |> testGenericIntervalCycle 12 5 
            , test "6-cycle" <|
                \_ -> 
                    [ [0,6], [1,7], [2,8], [3,9], [4,10], [5, 11] ]
                    |> List.map (List.map (\i -> Just i) )
                    |> testGenericIntervalCycle 12 6
            ]
        ]