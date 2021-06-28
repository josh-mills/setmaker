module Transformations exposing (..)

import Helpers exposing (rotationalArrays)
import List.Extra
import PcInt exposing (PcInt, edoToInt)
import PcSetBasics exposing (PcSet(..))


type Transformation
    = Transposition Int
    | Inversion Int



-- | M Int
-- | MI Int
-- transform : Transformation -> List PcInt


transformationToString : Transformation -> String
transformationToString t =
    case t of
        Transposition i ->
            "T_" ++ String.fromInt i

        Inversion i ->
            "I_" ++ String.fromInt i



-- M i ->
--     "M_" ++ String.fromInt i
-- MI i ->
--     "MI_" ++ String.fromInt i
{--the basic transformation structure should take
  - a list ordering function (identity for transposition, reverse for inversion)
    List PcInt -> List PcInt
  - a function to determine how to calcucalet a relationship between corresponding pcs
    Edo -> PcInt -> PcInt -> Int
  - a Transformation identifier (which transformation type are we going to return?)

  of course, these are a 1:1 with the transormation identifier, which should be convenient.
-}


findTransformationsOfType : Transformation -> PcSet -> PcSet -> List Transformation
findTransformationsOfType t (PcSet edoA pcsA) (PcSet edoB pcsB) =
    let
        orderList : List a -> List a
        orderList list =
            case t of
                Transposition _ ->
                    identity list

                Inversion _ ->
                    List.reverse list

        relatePCs : ( PcInt, PcInt ) -> Int
        relatePCs ( a, b ) =
            case t of
                Transposition _ ->
                    modBy (edoToInt edoA) (PcInt.pcIntToInt b - PcInt.pcIntToInt b)

                Inversion _ ->
                    modBy (edoToInt edoA) (PcInt.pcIntToInt a + PcInt.pcIntToInt b)

        indices : List Int
        indices =
            orderList pcsA
                |> rotationalArrays
                |> List.map (\l -> ( l, pcsB ))
                |> List.map (\( l1, l2 ) -> List.Extra.zip l1 l2)
                |> List.map (List.map relatePCs)
                |> List.map List.Extra.unique
                |> List.filter (\l -> List.length l == 1)
                |> List.filterMap (\l -> List.head l)
    in
    if edoA /= edoB then
        []

    else
        case t of
            Transposition _ ->
                List.map (\i -> Transposition i) indices

            Inversion _ ->
                List.map (\i -> Inversion i) indices


possibleTransformations : PcSet -> PcSet -> List Transformation
possibleTransformations (PcSet edoA pcsA) (PcSet _ pcsB) =
    {- TODO this is ugly as hell. name these functions useful things and then use them descriptively. -}
    let
        rotationsOfA : List (List PcInt)
        rotationsOfA =
            rotationalArrays pcsA

        --these next three functions should be combined into a single function
        allListPairs : List (List PcInt) -> List ( List PcInt, List PcInt )
        allListPairs rotations =
            List.map (\l -> ( l, pcsB )) rotations

        zipped : List ( List PcInt, List PcInt ) -> List (List ( PcInt, PcInt ))
        zipped listPairs =
            List.map (\( a, b ) -> List.Extra.zip a b) listPairs

        mapPcIntTupleToIntervals : List ( PcInt, PcInt ) -> List Int
        mapPcIntTupleToIntervals list =
            List.map (\( a, b ) -> PcInt.interval edoA a b) list

        isThisATransposition : List Int -> Maybe Transformation
        isThisATransposition list =
            let
                intervals : List Int
                intervals =
                    List.Extra.unique list
            in
            case intervals of
                h :: [] ->
                    Just (Transposition h)

                _ ->
                    Nothing

        isThisAnInversion : List Int -> Maybe Transformation
        isThisAnInversion list =
            let
                intervals : List Int
                intervals =
                    List.Extra.unique list
            in
            case intervals of
                h :: [] ->
                    Just (Inversion h)

                _ ->
                    Nothing

        transpositions : List Transformation
        transpositions =
            rotationalArrays pcsA
                --rotations of A
                |> List.map (\l -> ( l, pcsB ))
                -- list of all pairs of As to B (each element in a tuple is a list)
                |> List.map (\( a, b ) -> List.Extra.zip a b)
                -- zip into a list of tuples
                |> List.map (\l -> List.map (\( a, b ) -> PcInt.interval edoA a b) l)
                -- map each list of tuples onto a list of intervals
                |> List.filterMap isThisATransposition

        inversions : List Transformation
        inversions =
            List.reverse pcsA
                |> rotationalArrays
                |> List.map (\l -> ( l, pcsB ))
                |> List.map (\( a, b ) -> List.Extra.zip a b)
                |> List.map (\l -> List.map (\( a, b ) -> modBy (PcInt.edoToInt edoA) (PcInt.pcIntToInt a + PcInt.pcIntToInt b)) l)
                |> List.filterMap isThisAnInversion
    in
    transpositions ++ inversions



{- u
   transform : Transformation -> PcSet -> PcSet
-}
