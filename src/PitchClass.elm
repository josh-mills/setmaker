module PitchClass exposing (PitchClass, listFromInput, parsePitchClass, toInt, toString)

import Dict exposing (Dict)
import List.Extra exposing (uniqueBy)
import Regex exposing (Regex)


{-| This handles parsing and representation of pitch classes for 12-edo pitch class space.
PitchClass types currently support only 12-edo. More


# Creation

@docs listFromInput, parsePitchClass


# Common Helpers

@docs toInt, toString

-}
type PitchClass
    = PitchClass { note : Note, accidental : Accidental, pitchClassInteger : Int }


{-| Internal convenience constructor for PitchClass
-}
pc : Note -> Accidental -> PitchClass
pc note accidental =
    PitchClass { note = note, accidental = accidental, pitchClassInteger = calculatePcInt note accidental }


type Note
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Accidental
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp


{-| Build a single PitchClass from an input string.
-}
parsePitchClass : String -> Maybe PitchClass
parsePitchClass input =
    case String.toInt input of
        Just i ->
            fromInt i

        Nothing ->
            fromString input


fromInt : Int -> Maybe PitchClass
fromInt int =
    case int of
        0 ->
            Just (pc C Natural)

        1 ->
            Just (pc C Sharp)

        2 ->
            Just (pc D Natural)

        3 ->
            Just (pc E Flat)

        4 ->
            Just (pc E Natural)

        5 ->
            Just (pc F Natural)

        6 ->
            Just (pc F Sharp)

        7 ->
            Just (pc G Natural)

        8 ->
            Just (pc A Flat)

        9 ->
            Just (pc A Natural)

        10 ->
            Just (pc B Flat)

        11 ->
            Just (pc B Natural)

        _ ->
            Nothing


fromString : String -> Maybe PitchClass
fromString input =
    let
        maybeNote =
            String.toUpper input
                |> String.trim
                |> String.left 1
                |> noteFromString

        accidental =
            String.toLower input
                |> String.dropLeft 1
                |> accidentalFromString
    in
    Maybe.map (\note -> pc note accidental) maybeNote


noteFromString : String -> Maybe Note
noteFromString n =
    case n of
        "A" ->
            Just A

        "B" ->
            Just B

        "C" ->
            Just C

        "D" ->
            Just D

        "E" ->
            Just E

        "F" ->
            Just F

        "G" ->
            Just G

        _ ->
            Nothing


accidentalFromString : String -> Accidental
accidentalFromString a =
    case a of
        "bb" ->
            DoubleFlat

        "𝄫" ->
            DoubleFlat

        "♭♭" ->
            DoubleFlat

        "b" ->
            Flat

        "♭" ->
            Flat

        "-flat" ->
            Flat

        "#" ->
            Sharp

        "♯" ->
            Sharp

        "-sharp" ->
            Sharp

        "x" ->
            DoubleSharp

        "𝄪" ->
            DoubleSharp

        "##" ->
            DoubleSharp

        "♯♯" ->
            DoubleSharp

        _ ->
            Natural


noteToInt : Note -> Int
noteToInt note =
    case note of
        C ->
            0

        D ->
            2

        E ->
            4

        F ->
            5

        G ->
            7

        A ->
            9

        B ->
            11


accidentalToInt : Accidental -> Int
accidentalToInt accidental =
    case accidental of
        DoubleFlat ->
            -2

        Flat ->
            -1

        Natural ->
            0

        Sharp ->
            1

        DoubleSharp ->
            2


{-| Calculate the appropriate mod-12 pitch class integer for the given note and accidental
-}
calculatePcInt : Note -> Accidental -> Int
calculatePcInt note accidental =
    noteToInt note
        + accidentalToInt accidental
        |> modBy 12


{-| String representation of the pitch class. Uses unicode accidentals, but
does not include naturals, e.g.:
"C", "E♭", "F♯"
-}
toString : PitchClass -> String
toString (PitchClass { note, accidental }) =
    let
        letter =
            case note of
                C ->
                    "C"

                D ->
                    "D"

                E ->
                    "E"

                F ->
                    "F"

                G ->
                    "G"

                A ->
                    "A"

                B ->
                    "B"

        acc =
            case accidental of
                DoubleFlat ->
                    "𝄫"

                Flat ->
                    "♭"

                Natural ->
                    ""

                Sharp ->
                    "♯"

                DoubleSharp ->
                    "𝄪"
    in
    letter ++ acc


{-| Convert a PitchClass into the corresponding mod-12 pitch class integer.
-}
toInt : PitchClass -> Int
toInt (PitchClass { pitchClassInteger }) =
    pitchClassInteger


{-| Convert a user's input into a unique and ordered list of parsed PitchClass.
This will split on commas and white space for usual textual inputs.
It can also handle a duodecimal input string, e.g., "0134679A".
This only works where edo = 12.
-}
listFromInput : String -> List PitchClass
listFromInput input =
    let
        duodecimalCheck : Regex
        duodecimalCheck =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[\\daAbBtTeE]{2,}$"

        duodecimal : Bool
        duodecimal =
            Regex.contains duodecimalCheck (String.trim input)

        splitPoints : Regex
        splitPoints =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[,\\s]"
    in
    if duodecimal then
        String.split "" input
            |> List.map String.toUpper
            |> List.filterMap (\a -> Dict.get a duodecimalToDecimal)
            |> List.filterMap parsePitchClass
            |> List.sortBy toInt
            |> uniqueBy toInt

    else
        Regex.split splitPoints input
            |> List.filterMap parsePitchClass
            |> List.sortBy toInt
            |> uniqueBy toInt


duodecimalToDecimal : Dict String String
duodecimalToDecimal =
    Dict.fromList
        [ ( "0", "0" )
        , ( "1", "1" )
        , ( "2", "2" )
        , ( "3", "3" )
        , ( "4", "4" )
        , ( "5", "5" )
        , ( "6", "6" )
        , ( "7", "7" )
        , ( "8", "8" )
        , ( "9", "9" )
        , ( "A", "10" )
        , ( "B", "11" )
        , ( "T", "10" )
        , ( "E", "11" )
        ]
