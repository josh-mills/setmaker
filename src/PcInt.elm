module PcInt exposing
    ( listFromInput
    , invertPcInt, transposePcInt
    , toString
    , Edo, PcInt, edoFromInt, edoToInt, interval, intervalClass, pcInt, pcIntToInt
    )

{-| A general representation of a pitch class integer (PcInt) within a particular space (Edo).


# Creation

@docs listFromInput, parsePitchClass


# Transformations

@docs invertPcInt, transposePcInt


# Common Helpers

@docs toInt, toString

-}

import List.Extra
import Regex exposing (Regex)


{-| EDO is restricted to between 1 to 96 pitch classes, which should be quite enough.
-}
type Edo
    = Edo Int


edoFromInt : Int -> Edo
edoFromInt int =
    Edo <| clamp 1 96 int


edoToInt : Edo -> Int
edoToInt (Edo i) =
    i


type PcInt
    = PcInt Int


pcInt : Edo -> Int -> PcInt
pcInt edo int =
    PcInt (modBy (edoToInt edo) int)


pcIntToInt : PcInt -> Int
pcIntToInt (PcInt i) =
    i


toString : PcInt -> String
toString (PcInt a) =
    String.fromInt a


listFromInput : Edo -> String -> List PcInt
listFromInput edo input =
    let
        splitPoints : Regex
        splitPoints =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[,\\s]"
    in
    Regex.split splitPoints input
        |> List.filterMap String.toInt
        |> List.Extra.uniqueBy (\i -> modBy (edoToInt edo) i)
        |> List.sort
        |> List.map (pcInt edo)


{-| Interval (within the given EDO) between two pitch classes
-}
interval : Edo -> PcInt -> PcInt -> Int
interval (Edo i) (PcInt a) (PcInt b) =
    modBy i (b - a)


intervalClass : Edo -> PcInt -> PcInt -> Int
intervalClass edo pcA pcB =
    let
        i =
            interval edo pcA pcB
    in
    min i (edoToInt edo - i)


invertPcInt : Edo -> Int -> PcInt -> PcInt
invertPcInt edo index (PcInt p) =
    pcInt edo <| modBy (edoToInt edo) <| edoToInt edo - p + index


transposePcInt : Edo -> Int -> PcInt -> PcInt
transposePcInt edo index (PcInt p) =
    pcInt edo <| modBy (edoToInt edo) <| p + index
